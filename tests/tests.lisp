;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2016 Jéssica Milaré
;;; See the file license for license information.

(defpackage :storable-functions-tests
  (:use :cl :alexandria :storable-functions :lift :cl-store+functions :cl-store))

(in-package :storable-functions-tests)

(defun run-all-tests (&rest args)
  (apply #'run-cl-store+functions-tests args))

(defmacro ensure=-funcall (func1 func2 &rest args)
  "Ensures the functions func1 and func2 return values that are =."
  `(ensure-same (funcall ,func1 ,@args) (funcall ,func2 ,@args)
                :test #'= :ignore-multiple-values? t))

(defmacro ensure-equal-funcall (func1 func2 &rest args)
  "Ensures the functions func1 and func2 return values that are =."
  `(ensure-same (funcall ,func1 ,@args) (funcall ,func2 ,@args)
                :test #'equal :ignore-multiple-values? t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *all-standard-tests* nil))

(defmacro def-tester-for-standard-tests (testsuite-name options &body body)
  (declare (ignorable options))
  (with-unique-names (function-set-var n-function-set-var)
    `(progn
       ,@(loop
            for (testname function-vars function-set prologue-code current-body)
              in (reverse *all-standard-tests*)
            collect
              `(addtest (,testsuite-name)
                 ,testname
                 (flet ((get-current-function-set ()
                          ,function-set)
                        (get-current-function-set-disable-st-macro ()
                          ;; runs code as if st macro did nothing
                          (macrolet ((st (form &rest others)
                                       (declare (ignore others))
                                       form))
                            ,function-set))
                        (run-prologue-test-code (,function-set-var)
                          (destructuring-bind ,function-vars (ensure-list ,function-set-var)
                            (declare (ignorable ,@function-vars))
                            ,prologue-code))
                        (run-current-test (,function-set-var ,n-function-set-var)
                          (destructuring-bind ,function-vars (ensure-list ,function-set-var)
                            (destructuring-bind
                                  ,(mapcar
                                    #'(lambda (var)
                                        (symbolicate "N-"(symbol-name var)))
                                    function-vars) (ensure-list ,n-function-set-var)
                              ,@current-body))))
                   ,@body))))))

(defmacro def-std-test (testname (function-vars function-set) prologue-code
                        &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew '(,testname ,function-vars ,function-set ,prologue-code ,body)
              *all-standard-tests* :key #'car)
     ',testname))

(def-std-test simple-sum-lambda-test
    ((func) (list (st (lambda (a b)
                        (the fixnum (+ (the fixnum a)
                                       (the fixnum b)))))))
    ;; First the code that should be evaluated before the storage / restorage
    nil
  ;; Then the standard test code
  (ensure-cases (a b) '((2 3) (5 10) (14 11))
    (ensure=-funcall func n-func a b)))

;;; This test shows that a function that has been just restored
;;; can be seen as a copy of the old function, copying the closure variables
;;; as well.
;;; Do not forget to use (st (let ...)) or (st-let ...) instead of using
;;; (let ...)
(def-std-test increment-acc-test
    ((func) (list (st (let ((acc 0))
                        (declare (fixnum acc))
                        (st (lambda (&optional (diff 1))
                              (the fixnum (incf acc (the fixnum diff)))))))))
    nil
  ;; n-func is bound to the restored "copy" of the function
  (dotimes (i 3)
    (ensure=-funcall func n-func))
  (ensure-cases (x) '(16 -4 -18 5)
    (ensure=-funcall func n-func x)))

;;; Multiple closures work just fine as well
;;; IF they are stored in the same file and restored together (for cl-store).
(def-std-test increment-and-set-acc-test
    ((inc-func set-func) (st (let ((acc 0))
                               (declare (fixnum acc))
                               (list (st (lambda (&optional (diff 1))
                                           (incf acc (the fixnum diff))))
                                     (st (lambda (&optional (value 0))
                                           (setf acc (the fixnum value))))))))
    ;; Changing the value of acc BEFORE storage / restorage - closures should be saved
    ;; with the functions at the time they are stored
    (funcall set-func 8)
  ;; Now storage and restorage has taken place. Comparing two versions...
  (dotimes (i 3)
    (ensure=-funcall inc-func n-inc-func))
  (ensure=-funcall set-func n-set-func 5)
  (dotimes (i 3)
    (ensure=-funcall inc-func n-inc-func))
  (ensure=-funcall set-func n-set-func 10)
  (ensure-cases (x) '(19 -6 -2 3)
    (ensure=-funcall inc-func n-inc-func x))
  (ensure=-funcall set-func n-set-func 12))

;;; Recursive closures should work as well in the expected way
(def-std-test recursive-increment-and-set-acc-closure-test
    ((inner-inc1 inner-set1 outer-inc1 outer-set1
                 inner-inc2 inner-set2 outer-inc2 outer-set2)
     (st (let ((outer-acc 0))
           (nconc
            (st (let ((inner-acc 0))
                  (st (flet ((inner-inc (&optional (diff 1))
                               (incf inner-acc (the fixnum diff)))
                             (inner-set (value)
                               (setf inner-acc (the fixnum value)))
                             (outer-inc (&optional (diff 1))
                               (incf outer-acc (the fixnum diff)))
                             (outer-set (value)
                               (setf outer-acc (the fixnum value))))
                        (list #'inner-inc #'inner-set
                              #'outer-inc #'outer-set)))))
            (st (let ((inner-acc 0))
                  (st (flet ((inner-inc (&optional (diff 1))
                               (incf inner-acc (the fixnum diff)))
                             (inner-set (value)
                               (setf inner-acc (the fixnum value)))
                             (outer-inc (&optional (diff 1))
                               (incf outer-acc (the fixnum diff)))
                             (outer-set (value)
                               (setf outer-acc (the fixnum value))))
                        (list #'inner-inc #'inner-set
                              #'outer-inc #'outer-set)))))))))
    (progn ;; Changing the values of the closures variables
      (funcall inner-set1 19)
      (funcall inner-set2 7)
      (funcall outer-set1 1))
  ;; set 1 of functions
  (dotimes (i 3)
    (ensure=-funcall inner-inc1 n-inner-inc1))
  (ensure=-funcall inner-set1 n-inner-set1 3)
  (ensure=-funcall outer-set1 n-outer-set1 14)
  (dotimes (i 3)
    (ensure=-funcall outer-inc1 n-outer-inc1))
  ;; set 2 of functions
  (dotimes (i 3)
    (ensure=-funcall inner-inc2 n-inner-inc2))
  (ensure=-funcall inner-set2 n-inner-set2 17)
  (ensure=-funcall outer-set2 n-outer-set2 8)
  (dotimes (i 3)
    (ensure=-funcall outer-inc2 n-outer-inc2))
  ;; mixed set 1 and 2
  (ensure=-funcall outer-set2 n-outer-set2 9)
  (dotimes (i 3)
    (ensure=-funcall outer-inc1 n-outer-inc1)
    (ensure=-funcall outer-inc2 n-outer-inc2))
  (dotimes (i 3)
    (ensure=-funcall inner-inc1 n-inner-inc1)
    (ensure=-funcall inner-inc2 n-inner-inc2))
  ;; other increment values
  (ensure-cases (inn1 out1 inn2 out2)
      '((3 0 7 1) (11 -5 4 -3) (-2 8 -9 -15) (2 -13 16 11))
    (ensure=-funcall inner-inc1 n-inner-inc1 inn1)
    (ensure=-funcall outer-inc2 n-outer-inc2 out2)
    (ensure=-funcall inner-inc2 n-inner-inc2 inn2)
    (ensure=-funcall outer-inc1 n-outer-inc1 out1)))

;;; No problem with multiple variables as well
(def-std-test shifting-closure-test
    ((func) (list (st (let ((a 0) (b 1) (c 2))
                        (declare (fixnum a b c))
                        ;; This function returns the value received 3 calls ago.
                        (st (lambda (value)
                              (shiftf a b c (the fixnum value))))))))
    (dolist (x '(3 11 10 8 16))
      (funcall func x))
  (ensure-cases (x) '(10 6 -13 -7 10 13 -15 -18 0 0 0)
    (ensure=-funcall func n-func x)))

(def-std-test recursive-shifting-closure-test
    ((func1 func2) (st (let ((a 0) (b 1))
                         (declare (fixnum a b))
                         ;; Another interesting example.
                         (list
                          (st (let ((c 2) (d 3))
                                (declare (fixnum c d))
                                (st (lambda (value)
                                      (shiftf a b c d (the fixnum value))))))
                          (st (let ((c 2) (d 3))
                                (declare (fixnum c d))
                                (st (lambda (value)
                                      (shiftf a b c d (the fixnum value))))))))))
    (progn
      (dolist (x '(6 18 6))
        (funcall func1 x))
      (dolist (x '(8 14 0))
        (funcall func2 x)))
  (ensure-cases (x) '(2 0 -13)
    (ensure=-funcall func1 n-func1 x))
  (ensure-cases (x) '(2 0 -13)
    (ensure=-funcall func1 n-func1 x))
  (ensure-cases (x) '(-8 8)
    (ensure=-funcall func2 n-func2 x))
  (ensure-cases (x) '(15 -3)
    (ensure=-funcall func1 n-func1 x))
  (ensure-cases (x) '(4 -1 18)
    (ensure=-funcall func2 n-func2 x))
  (ensure-cases (x) '(0 0)
    (ensure=-funcall func1 n-func1 x))
  (ensure-cases (x) '(0 0 0)
    (ensure=-funcall func2 n-func2 x)))

(def-std-test labels-add-sub-test
    ((set-acc add-sub sub-add add-and-sub)
     (st (let ((acc 0))
           (declare (fixnum acc))
           (cons (st #'(lambda (x)
                         (setf acc x)))
                 (st (labels ((add-sub (values)
                                (if values
                                    (cons (+ (the fixnum (car values)) acc)
                                          (the list (sub-add (cdr values))))))
                              (sub-add (values)
                                (if values
                                    (cons (- (the fixnum (car values)) acc)
                                          (the list (add-sub (cdr values)))))))
                       (list #'add-sub #'sub-add
                             (st #'(lambda (values1 values2)
                                     (list (add-sub values1)
                                           (sub-add values2)))))))))))
    (funcall set-acc 11)
  (ensure-cases (as sa aas1 aas2 set)
      '(((16 12 11 19 4 7 0 8) (10 17 14 4 11) (14 13 5) (15 0 12 9) 4)
        ((4 9 3 16 13 17) (3 5 9 15 11 10 4 18 9) (15 11 17 3 13 7) (19 1) 12)
        ((3 13 2 13) (3 9 7 11 8 16) (9 19 6 7 7 12 9 19 15) (1 6 7 12 14 16) 15)
        ((4 3) (6 11 14 16 9 10 1 3 6) (7 1 1) (11 11 4 18 14 0 15 9 4) 9))
    (ensure-equal-funcall add-sub n-add-sub as)
    (ensure-equal-funcall sub-add n-sub-add sa)
    (ensure-equal-funcall add-and-sub n-add-and-sub aas1 aas2)
    (ensure-equal-funcall set-acc n-set-acc set)))

;;; The st macro can mark any form that should return a function;
;;; e.g., compose, complement, adjoin...
;;; When requested to save the returned function, the function name and
;;; arguments are saved.
;;; After restorage, the same function is called again with the "same" arguments
;;; (i.e., a copy of the arguments) in the corresponding lexical closure.
(def-std-test st-compose-test ((func) (list (st (compose #'cadr #'reverse))))
    nil
  (ensure (funcall n-func (list nil nil nil nil nil t nil))))

(def-std-test st-curry-test ((func) (list (st (curry #'list 1 2 3 4))))
    nil
  (ensure-same (funcall n-func 5 6) '(1 2 3 4 5 6)
               :test #'equal))

(def-std-test stq-compose-test ((func) (list (stq (compose #'cadr #'reverse))))
    nil
  (ensure (funcall n-func (list nil nil nil nil nil t nil))))

(def-std-test stq-curry-test ((func) (list (stq (curry #'list 1 2 3 4))))
    nil
  (ensure-same (funcall n-func 5 6) '(1 2 3 4 5 6)
               :test #'equal))

(def-std-test unsafe-let*-call
    ((func) (st (let* ((a 32)
                       (fun (st (lambda (x)
                                  (+ x a)))))
                  fun)))
    nil
  (ensure-cases (x) '(6 45 41 6 48 45 11 31 15 4)
    (ensure-equal-funcall func n-func x)))
