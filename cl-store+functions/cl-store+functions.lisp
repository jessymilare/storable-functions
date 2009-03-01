;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :cl-store+functions
  (:use :cl :cl-store :storable-functions :metatilities)
  (:export #:cl-store+functions))

(in-package :cl-store+functions)

(defconstant +code-information-code+ 250)

(defbackend cl-store+functions
    :magic-number 1025429561
    ;; cl-store+functions is meant to be compatible with cl-store
    :compatible-magic-numbers (1279478851 1395477571)
    :old-magic-numbers (1912923 1886611788 1347635532 1886611820 1414745155
				1349740876 1884506444 1347643724 1349732684 1953713219
				1416850499 1395477571)
    :extends (cl-store)
    :fields ((restorers :accessor restorers
			:initform (let ((cl-store-backend (find-backend 'cl-store))
					(hash-table (make-hash-table :size 100)))
				    (maphash #'(lambda (k v)
						 (setf (gethash k hash-table) v))
					     (restorers cl-store-backend))
				    (setf (gethash +code-information-code+ hash-table)
					  'code-information)
				    hash-table))))

;;; Now, tell me if it ain't a piece of cake to store functions in a file?
;;; It even leaves me time to take a little nap after doing this ;)

(defstore-cl-store+functions (func function stream)
  (let ((obj (get-function-referrer func)))
    (if obj
	;; A little hack. We don't want the backend-restore-object methods to check
	;; circularities for obj since possible circularities for the function "func"
	;; was already handled by the resolving-backend.
	;; FIXME - This hack use internal information about cl-store not in a cool way,
	;; but no alternative was found - unless explictly call functions store-type-object
	;; and restore-type-object.
	(cl-store::internal-store-object *current-backend* obj stream)
	;; No information about the function was provided by the user (e.g. macro st was not used)
	;; giving up to cl-store standard storable methods (which stores the function name)
	(call-next-method))))

(defstore-cl-store+functions (obj code-information stream)
  ;; store-code-info receives the object to be stored and a callback
  ;; which receives no arguments and should actually store the object.
  (cl-store::output-type-code +code-information-code+ stream)
  (store-code-info obj #'call-next-method))

(defrestore-cl-store+functions (code-information stream)
  ;; Another little hack. During storage, the method for code information used call-next-method
  ;; for storing the class. This means that the instance for 'code-information does not count as a new
  ;; referrenciable value, since it its the same object restored by this method.
  ;; FIXME - see FIXME above.
  (let ((obj (cl-store::internal-restore-object
	      *current-backend* (get-next-reader *current-backend* stream) stream)))
    ;; restore-code-info receives the object that was restored
    (restore-code-info obj)))

(defmethod backend-store ((backend cl-store+functions) (stream stream) obj)
  ;; cl-store is a save / load objects library.
  ;; The storage / restorage of the objects are modular.
  ;; In this case, we need to encapsulate the storage to undo side effects.
  ;; and to fix some references (in resolving-backend style)
  ;; Elephant, for instance, would not use this macro at all.
  (with-storable-functions-storage ()
    ;; Circular checking is REQUIRED - not only because the code of the functions
    ;; could contain gensyms. Closure internal representation is not exactly circular,
    ;; but it contains various references to the same object, and these references
    ;; are meant to be eq.
    (let ((cl-store:*check-for-circs* t))
      (call-next-method))))

(defmethod backend-restore ((backend cl-store+functions) (stream stream))
  (with-storable-functions-restorage ()
    (let ((cl-store:*check-for-circs* t)
	  (cl-store::*restorers* (restorers backend)))
      (call-next-method))))
