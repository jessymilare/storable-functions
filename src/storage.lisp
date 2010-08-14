;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

;;; About the approach here:
;;; It is needed to avoid circularity on closures because the code needs to be
;;; generated right after restorage.
;;; So, during storage, the environment slot is removed from every instance of a
;;; subclass of code-information.
;;; This slot will be safelly recoverred during restorage. This way there is no
;;; internal circularity
;;; from the root down to all the leaves.
;;;
;;; The instances of class "function-referrer" are created to be stored instead
;;; of the functions; each instance contains the root of the environment tree
;;; and the function-info instance associated with the function.
;;; Unless something really goes wrong, right after restorage of a instance of
;;; "function-referrence", the entire environment tree will be available without
;;; circularities from the root and then it will be possible to generate the code.

(in-package :storable-functions)

(defclass function-referrer (code-information)
  ((function-info :accessor info-function-info :initarg :function-info :type function-info)
   (root :accessor info-root :initarg :root :type code-information)))

;;; During restorage, it is not good to keep weak pointers to the children of closures.
;;; So we keep a list of weaklists that need to be set at the end (see utils.lisp).
(defvar *weak-lists-to-set* nil)

(defmacro with-storable-functions-restorage ((&key (restorage-table :create)
                                                   &allow-other-keys)
					     &body body)
  `(let ((*restored-functions* ,(ecase restorage-table
				       (:create '(make-hash-table))
				       (:clear '(clrhash *restored-functions*))
				       (:reuse '*restored-functions*)))
	 (*weak-lists-to-set* nil))
     (prog1 (progn ,@body)
       (mapcar #'set-weak-list *weak-lists-to-set*))))

(defmacro with-storable-functions-storage ((&key (execute-gc t)
                                                 &allow-other-keys)
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
        ;; avoids circularity - the circularity is well known,
        ;; it is restored after call to restore-code-info
	(slot-makunbound info 'environment)
	(unwind-protect
	     (funcall callback)
	  (setf (info-environment info) env))))))

(defmethod store-code-info ((info function-referrer) callback)
  (funcall callback))

(defmethod store-code-info ((info closure-info) callback)
  (bt:with-recursive-lock-held (*storage-lock*)
    ;; avoids implementation-dependent details in the storage
    (unset-weak-list (info-children-weak-list info))
    (unwind-protect (call-next-method)
      ;; now restores the list of weak-pointers
      (set-weak-list (info-children-weak-list info)))))

(defmethod store-code-info ((info let-closure-info) callback)
  (let ((func (info-values-accessor info)))
    (bt:with-recursive-lock-held (*storage-lock*)
      ;; unbounds the unstorable function slot
      (slot-makunbound info 'values-accessor)
      ;; takes a "snapshot" of the current closure status
      (setf (info-values info) (funcall func info))
      (unwind-protect (call-next-method)
        ;; removes unnecessary information
	(slot-makunbound info 'values)
        ;; and rebinds the function slot
	(setf (info-values-accessor info) func)))))

(defgeneric restore-code-info (info)
  (:method ((info code-information))
    (unless (slot-boundp info 'environment)
      (setf (info-environment info) nil))
    info))

(defmethod restore-code-info ((info function-referrer))
  (setf (info-environment info) nil
	(info-environment (info-root info)) nil)
  (let* ((function-info (info-function-info info))
	 (func (get-info-value function-info)))
    (setf (get-function-info func) function-info)
    func))

(defmethod restore-code-info ((info closure-info))
  (prog1 (call-next-method)
    (let ((children (get-list-from-weak-list (info-children-weak-list info))))
      (push (info-children-weak-list info) *weak-lists-to-set*)
      (dolist (child children)
	(setf (info-environment child) info)
	#+ignore
	(restore-code-info child))
      ;; Rebuilds the list of weak pointers
      (setf (info-children info) children))))

(defmethod restore-code-info ((info let-closure-info))
  (prog1 (call-next-method)
    (setf (info-values-accessor info) (get-info-value info))
    (slot-makunbound info 'values)))

(defmethod restore-code-info ((info flet-closure-info))
  (prog1 (call-next-method)
    ;; the standard method will set info-environment to nil if
    ;; the 'environment slot is unbound
    (let ((environment
	   (ecase (info-type info)
	     (labels info)
	     (flet (info-environment info)))))
      (dolist (function (info-functions info))
	(setf (info-environment function) environment)
	#+ignore
	(restore-code-info function)))))
