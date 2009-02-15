;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file licence for licence information.

(in-package :storable-functions)

(defun parse-body (body)
  (loop for rest on body
     for form = (car rest)
     while (eq 'declare (first form))
       collect form into declarations
     finally (return (values rest declarations))))

(defmacro with-collector ((collector) &body body)
  (with-unique-names (list last)
    `(let ((,list nil) (,last nil))
       (flet ((,collector (elt)
		(if ,list
		    (setf (cdr ,last) (cons elt nil))
		    (setf ,last (setf ,list (cons elt nil))))
		,list))
	 ,@body)
       ,list)))
