;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file licence for licence information.

;;; About the approach here:
;;; It is needed to avoid circularity on closures because the code needs to be generated
;;; right after restorage.
;;; So, during storage, the environment slot is removed from every instance of a subclass of code-information.
;;; This slot will be safelly recoverred during restorage. This way there is no internal circularity
;;; from the root down to all the leaves.
;;;
;;; The instances of class "function-referrer" are created to be stored in the place of functions;
;;; each instance contains the root of the environment tree and the function-info instance
;;; associated with the function.
;;; Unless something really goes wrong, right after restorage of a instance of "function-referrence",
;;; the entire environment tree will be available without circularities
;;; from the root and then it will be possible to generate the code.

(in-package :storable-functions)

(defclass function-referrer (code-information)
  ((function-info :accessor info-function-info :initarg :function-info :type function-info)
   (root :accessor info-root :initarg :root :type code-information)))

(defmacro with-storable-functions-restorage ((&key (restorage-table :create) &allow-other-keys)
					     &body body)
  `(let ((*restored-functions* ,(ecase restorage-table
				       (:create '(make-hash-table))
				       (:clear '(clrhash *restored-functions*))
				       (:reuse '*restored-functions*))))
     ,@body))

(defmacro with-storable-functions-storage ((&key (execute-gc t) &allow-other-keys)
					   &body body)
  `(progn
     ;; GC avoids to store functions which aren't around anymore.
     (when ,execute-gc
       (tg:gc))
     ,@body))

(defun get-function-referrer (function)
  (let ((info (get-function-info function)))
    (when info
      (make-instance 'function-referrer
		     :root (find-root-info info)
		     :function-info info))))

(defgeneric store-code-info (info callback)
  (:method ((info code-information) callback)
    (let ((env (info-environment info)))
      (bt:with-recursive-lock-held (*storage-lock*)
	(slot-makunbound info 'environment) ; avoids circularity - the circularity is well known,
					; it is restored in call to restore-code-info
	(funcall callback)
	(setf (info-environment info) env)))))

(defmethod store-code-info ((info function-referrer) callback)
  ;; Avoids the standard method to unbound the environment slot
  (funcall callback))

(defmethod store-code-info ((info let-closure-info) callback)
  (let ((func (info-values-generator info)))
    (bt:with-recursive-lock-held (*storage-lock*)
      (slot-makunbound info 'values-generator) ; unbounds the unstorable function slot
      (setf (info-values info) (funcall func info)) ; takes a "snapshot" of the current closure status
      (unwind-protect (call-next-method)
	(slot-makunbound info 'values) ; removes unnecessary information
	(setf (info-values-generator info) func))))) ; and rebinds the function slot

(defmethod store-code-info ((info closure-info) callback)
  (let ((children (info-children info)))
    (bt:with-recursive-lock-held (*storage-lock*)
      (setf (info-children-weak-list info) children) ; (avoid implementation-dependent details in the storage)
      (unwind-protect (call-next-method)
	(setf (info-children info) children))))) ; now restores the list of weak-pointers

(defgeneric restore-code-info (info)
  (:method ((info code-information))
    (unless (slot-boundp info 'environment)
      (setf (info-environment info) nil))
    info))

(defmethod restore-code-info ((info function-referrer))
  (setf (info-environment info) nil)
  (let* ((function-info (info-function-info info))
	 (func (get-remove-info-value function-info)))
    (setf (get-function-info func) function-info)
    func))

(defmethod restore-code-info ((info closure-info))
  (let ((children (info-children-weak-list info)))
    (dolist (child children)
      (setf (info-environment child) info)
      (restore-code-info child))
    ;; Rebuilds the list of weak pointers
    (setf (info-children info) children))
  (call-next-method))

(defmethod restore-code-info ((info let-closure-info))
  (setf (info-values-generator info) (get-remove-info-value info))
  (slot-makunbound info 'values)
  (call-next-method))

(defmethod restore-code-info ((info flet-closure-info))
  (prog1 (call-next-method) ; calls standard method which will set info-environment to nil if
					; the 'environment slot is unbound
    (let ((environment
	   (ecase (info-type info)
	     (labels info)
	     (flet (info-environment info)))))
      (dolist (function (info-functions info))
	(setf (info-environment function) environment)
	(restore-code-info function)))))
