;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file licence for licence information.

(in-package :storable-functions)

(defvar *storable-function-table* (tg:make-weak-hash-table :weakness :key :test #'eq))

(defvar *id* 0)
(defvar *restored-functions* (tg:make-weak-hash-table :weakness :value))

;;; We need to lock because we change the field values in storage / restorage.
;;; This is just for safety, because (I believe) there probably won't be
;;; concurrent accesses at all. That's why the lock is a global variable,
;;; and not a field inside each class instance.

(defvar *storage-lock* (bt:make-recursive-lock "Storable Functions Storage Lock"))

(declaim (inline get-function-info (setf get-function-info) rem-function-info))

(defun get-function-info (func)
  (gethash func *storable-function-table*))

(defun (setf get-function-info) (info func)
  (setf (gethash func *storable-function-table*) info))

(defun rem-function-info (func)
  (remhash func *storable-function-table*))

(defclass code-information ()
  ((environment :initarg :environment :accessor info-environment
		:initform nil :type (or code-information null))))

(defun setting-info-value (info value)
  `(setf (gethash ',info *restored-functions*) ,value))

(defun get-remove-info-value (info)
  (prog1 (or (gethash info *restored-functions*)
	     (progn
	       (eval (generate-code info))
	       ;; If everything went ok, this should return a value
	       (gethash info *restored-functions*)))
    (remhash info *restored-functions*)))

(defun peek-info-value (info)
  (gethash info *restored-functions*))

;;; Functions

(defclass function-info (code-information)
  ())

(defclass lambda-info (function-info)
  ((body :initarg :body :accessor info-body :type list)
   (lambda-list :initarg :lambda-list :accessor info-lambda-list :type list)))

(defclass named-lambda-info (lambda-info)
  ((name :initarg :name :accessor info-name :type symbol)))

(defclass function-call-info (function-info)
  ((function-name :initarg :function-name :accessor info-function-name :type symbol)
   (values :initarg :values :accessor info-values :type list)))

;;; Closures

(defclass closure-info (code-information)
  ((type :initarg :type :accessor info-type :type symbol)
   (children :accessor info-children-weak-list :type list :initform nil)
   (declarations :initarg :declarations :accessor info-declarations :type list)))

(defclass let-closure-info (closure-info)
  ((values-generator :initarg :values-generator :accessor info-values-generator :type function)
   (variables :initarg :variables :accessor info-variables :type list)
   (values :initarg :values :accessor info-values :type list)))

(defclass flet-closure-info (closure-info)
  ((functions :initarg :functions :accessor info-functions :type list)))

(defclass macro-closure-info (closure-info)
  ((macros :initarg :macros :accessor info-macros :type list)))

(defmethod initialize-instance :after ((info code-information) &key)
  (let ((env (info-environment info)))
    (when (typep env 'closure-info)
      (pushnew (tg:make-weak-pointer info)
	       (info-children-weak-list env)
	       :key #'weak-pointer-value))))

(defmethod (setf info-environment) :before (environment (info code-information))
  (when (slot-boundp info 'environment)
    (let ((env (info-environment info)))
      (when (typep env 'closure-info)
	(setf (info-children-weak-list env)
	      (delete info (info-children-weak-list env)
		      :key #'tg:weak-pointer-value))))))

(defmethod (setf info-environment) :after ((environment closure-info) (info code-information))
  (pushnew (tg:make-weak-pointer info)
	   (info-children-weak-list environment)
	   :key #'weak-pointer-value))

(defmethod info-children ((info closure-info))
  ;; We put weak pointers here because the children of a closure
  ;; are only kept for generating the child code.
  ;; If some child can be garbage-collected, it means it's function(s) is(are) not around anymore,
  ;; therefore the child don't need to be stored.
  (with-collector (collect)
    (setf (info-children-weak-list info)
	  (delete-if #'(lambda (child)
			 (not (if child (collect child))))
		     (info-children-weak-list info)
		     :key #'tg:weak-pointer-value))))

(defmethod (setf info-children) (children (info closure-info))
  (setf (info-children-weak-list info)
	(mapcar #'make-weak-pointer children))
  children)

(macrolet ((def (method class)
	     `(progn
		(defmethod ,method :around ((info ,class))
		  (bt:with-recursive-lock-held (*storage-lock*)
		   (call-next-method)))
		(defmethod (setf ,method) :around (value (info ,class))
		  (bt:with-recursive-lock-held (*storage-lock*)
		    (call-next-method))))))

  (def info-values-generator let-closure-info)
  (def info-values let-closure-info))

(defun find-root-info (info)
  (let ((par (info-environment info)))
    (if (not par)
	info
	(find-root-info par))))

(defun generate-code (info)
  (bt:with-recursive-lock-held (*storage-lock*)
    (generate-code-from-info (find-root-info info))))

(defun generate-closure-values-generator (variables)
  `(compile nil (lambda (internal-info) ; internal-info is the same as info, but for run-time access
					; this is to make future expansions (e.g. run time checking
					; which variables can be stored)
		  (declare (ignore internal-info))
		  (list . ,variables))))

(defgeneric generate-code-from-info (info))

(defmethod generate-code-from-info ((info lambda-info))
  (setting-info-value info `(compile nil ,(generate-function-lambda info))))

(defgeneric generate-function-lambda (info))

(defmethod generate-function-lambda ((info lambda-info))
  `(lambda ,(info-lambda-list info)
     . ,(info-body info))) ; avoid doing unnecessary consing - this code is just for a single evaluation

(defmethod generate-function-lambda ((info named-lambda-info))
  `(named-lambda ,(info-name info) ,(info-lambda-list info)
     . ,(info-body info)))

(defmethod generate-code-from-info :around ((info closure-info))
  `(,(info-type info) ,(generate-closure-args info)
     (declare ,@(info-declarations info))
     ,(call-next-method)))

(defmethod generate-code-from-info ((info closure-info))
  `(progn . ,(mapcar #'generate-code-from-info (info-children info))))

(defmethod generate-code-from-info ((info let-closure-info))
  `(progn
     ,(setting-info-value
       info (generate-closure-values-generator (info-variables info)))
     ,(call-next-method)))

(defmethod generate-code-from-info ((info flet-closure-info))
  `(progn
     ,@(mapcar #'(lambda (func)
		   (setting-info-value func `(function ,(info-name func))))
	       (info-functions info))
     ,(call-next-method)))

(defun generate-closure-args (info)
  (ecase (info-type info)
    ((let let*)
     (mapcar #'(lambda (var value)
		 `(,var ',value))
	     (info-variables info) (if (slot-boundp info 'values)
				       (info-values info)
				       (funcall (info-values-generator info) info))))
    ((flet labels)
     (mapcar (compose #'cdr #'generate-function-lambda)
	     (info-functions info)))
    ((macrolet symbol-macrolet)
     (info-macros info))))

(defmethod generate-code-from-info ((info function-call-info))
  (setting-info-value info `(,(car (info-function-name info)) . ,(info-values info))))
