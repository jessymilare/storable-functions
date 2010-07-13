;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :storable-functions)

(defun parse-body (body)
  (loop for rest on body
     for form = (car rest)
     while (eq 'declare (first form))
       collect form into declarations
     finally (return (values rest declarations))))

(defmacro with-collector ((collector) &body body)
  (with-gensyms (list last)
    `(let ((,list nil) (,last nil))
       (flet ((,collector (elt)
		(if ,list
		    (setf (cdr ,last) (cons elt nil))
		    (setf ,last (setf ,list (cons elt nil))))
		,list))
	 ,@body)
       ,list)))

(defmacro dlambda (preargs &body ds)
  (with-gensyms (args)
    `(lambda (,@preargs &rest ,args)
       ,@(loop while (or (stringp (car ds))
                         (and (listp (car ds))
                              (eq 'declare (caar ds))))
              collect (pop ds))
       (case (car ,args)
         ,@(mapcar
            (lambda (d)
              `(,(if (eq t (car d))
                     t
                     (ensure-list (car d)))
                 (apply (lambda ,@(cdr d))
                        ,(if (eq t (car d))
                             args
                             `(cdr ,args)))))
            ds)))))

(declaim (inline #+ignore safe-weak-pointer-value pushnew-weak-list))

#+ignore
(defun safe-weak-pointer-value (pointer)
  (if (tg:weak-pointer-p pointer)
      (tg:weak-pointer-value pointer)
      pointer))

(defun pushnew-weak-list (elt list)
  (if (car list)
      (pushnew (tg:make-weak-pointer elt) (cdr list) :key #'tg:weak-pointer-value)
      (pushnew elt (cdr list)))
  list)

(defun delete-weak-list (elt list)
  (setf (cdr list)
	(if (car list)
	    (delete elt (cdr list) :key #'tg:weak-pointer-value)
	    (delete elt (cdr list))))
  list)

(defun get-list-from-weak-list (list)
  (if (car list)
      (with-collector (collect)    
	(setf (cdr list)
	      (delete-if #'(lambda (child)
			     (not (when child
				    (collect child)
				    t)))
			 (cdr list)
			 :key #'tg:weak-pointer-value)))
      (cdr list)))

(declaim (inline set-weak-list unset-weak-list new-weak-list))

(defun set-weak-list (weak-list &optional (list (cdr weak-list) listp))
  (if (or listp (not (car weak-list)))
      (setf (cdr weak-list) (mapcar #'tg:make-weak-pointer list)))
  (setf (car weak-list) t)
  list)

(defun unset-weak-list (weak-list &optional (list (get-list-from-weak-list weak-list)))
  (setf (car weak-list) nil
	(cdr weak-list) list))

(defun new-weak-list ()
  (cons t nil))
