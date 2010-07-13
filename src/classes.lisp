;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :storable-functions)

(defvar *storable-function-table* (tg:make-weak-hash-table :weakness :key :test #'eq
                                                           :weakness-matters nil))

(defvar *id* 0)
(defvar *restored-functions* (tg:make-weak-hash-table :weakness :key :test #'eq
                                                      :weakness-matters nil))

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

(defun get-info-value (info)
  (prog1
      (or (gethash info *restored-functions*)
          (progn
            (eval (generate-code info))
            ;; If everything went ok, this should return a value now
            (gethash info *restored-functions*)))
    (remhash info *restored-functions*)))

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

(defclass quoted-function-info (function-info)
  ((body :initarg :body :accessor info-body :type list)))

;;; Closures

(defclass closure-info (code-information)
  ((type :initarg :type :accessor info-type :type symbol)
   (children :accessor info-children-weak-list :type list :initform (new-weak-list))
   (declarations :initarg :declarations :accessor info-declarations :type list)))

(defclass let-closure-info (closure-info)
  ((values-accessor :initarg :values-accessor :accessor info-values-accessor :type function)
   (variables :initarg :variables :accessor info-variables :type list)
   (values :initarg :values :accessor info-values :type list)))

(defclass flet-closure-info (closure-info)
  ((functions :initarg :functions :accessor info-functions :type list)))

(defclass macro-closure-info (closure-info)
  ((macros :initarg :macros :accessor info-macros :type list)))

(defmethod initialize-instance :after ((info code-information) &key)
  (let ((env (info-environment info)))
    (when (typep env 'closure-info)
      (setf (info-children-weak-list env)
	    (pushnew-weak-list info (info-children-weak-list env))))))

(defmethod (setf info-environment) :before (environment (info code-information))
  (when (slot-boundp info 'environment)
    (let ((env (info-environment info)))
      (when (typep env 'closure-info)
	(setf (info-children-weak-list env)
	      (delete-weak-list info (info-children-weak-list env)))))))

(defmethod (setf info-environment) :after (environment (info code-information))
  (let ((env (info-environment info)))
    (when (typep env 'closure-info)
      (setf (info-children-weak-list env)
	    (pushnew-weak-list info (info-children-weak-list env))))))

(defgeneric info-children (info))

(defmethod info-children ((info closure-info))
  ;; We put weak pointers here because the children of a closure
  ;; are only kept for generating the child code.
  ;; If some child can be garbage-collected, it means it's function(s) is(are) not around anymore,
  ;; therefore the child don't need to be stored.
  (get-list-from-weak-list (info-children-weak-list info)))

(defgeneric (setf info-children) (children info))

(defmethod (setf info-children) (children (info closure-info))
  (set-weak-list (info-children-weak-list info) children))

;;; Define locking methods for accessors.
(macrolet ((def (method class)
	     `(progn
		(defmethod ,method :around ((info ,class))
		  (bt:with-recursive-lock-held (*storage-lock*)
		   (call-next-method)))
		(defmethod (setf ,method) :around (value (info ,class))
		  (bt:with-recursive-lock-held (*storage-lock*)
		    (call-next-method))))))
  (def info-values-accessor let-closure-info)
  (def info-values let-closure-info))

(defun find-root-info (info)
  (let ((par (and (slot-boundp info 'environment)
		  (info-environment info))))
    (if (null par)
	info
	(find-root-info par))))

(defun generate-code (info)
  (bt:with-recursive-lock-held (*storage-lock*)
    (generate-code-from-info (find-root-info info))))

(defun maybe-compile (function)
  ;; compiles function if possible.
  (or (ignore-errors (compile nil function))
      function))

(defun generate-closure-values-accessor (variables)
  (with-gensyms (sym info local-p value parent)
    `(maybe-compile (dlambda (,info)
                      (:get (,sym &optional ,local-p)
                            (case ,sym
                              ,@(mapcar (lambda (var)
                                          `(,var (values ,var t)))
                                        variables)
                              (t (if-let ((,parent (and (not ,local-p)
                                                        (info-environment ,info))))
                                   (funcall (info-values-accessor ,parent)
                                            ,parent :get ,sym)
                                   (values nil nil)))))
                      (:set (,sym ,value &optional ,local-p)
                            (case ,sym
                              ,@(mapcar (lambda (var)
                                          `(,var (values (setf ,var ,value) t)))
                                        variables)
                              (t (if-let ((,parent (and (not ,local-p)
                                                        (info-environment ,info))))
                                   (funcall (info-values-accessor ,parent)
                                            ,parent :set ,sym ,value)
                                   (values nil nil)))))
                      (t () (list . ,variables))))))

(defgeneric generate-code-from-info (info))

(defmethod generate-code-from-info ((info lambda-info))
  (setting-info-value info `(maybe-compile ,(generate-function-lambda info))))

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
       info (generate-closure-values-accessor (info-variables info)))
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
				       (funcall (info-values-accessor info) info))))
    ((flet labels)
     (mapcar (compose #'cdr #'generate-function-lambda)
	     (info-functions info)))
    ((macrolet symbol-macrolet)
     (info-macros info))))

(defmethod generate-code-from-info ((info function-call-info))
  (setting-info-value
   info
   (let* ((values (info-values info))
	  (vars (loop repeat (length values)
		     collect (gensym))))
     (declare (type list vars)
	      (dynamic-extent vars))
     `(let* ,(mapcar #'list vars
		     ;; quoted values - some value can be a list
		     (mapcar (curry #'list 'quote) values))
	(,(info-function-name info) ,@vars)))))

(defmethod generate-code-from-info ((info quoted-function-info))
  (setting-info-value info (info-body info)))
