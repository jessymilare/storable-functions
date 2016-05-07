;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2016 Jéssica Milaré
;;; See the file license for license information.

(in-package :storable-functions-tests)

(defun run-cl-store+functions-tests (&rest args)
  (apply #'run-tests :suite 'test-cl-store+functions args))

(defvar *test-file* #P".storable-functions.cls")

(deftestsuite test-cl-store+functions ()
  ((cl-store-test-file #P".cl-store+functions-test-file"))
  (:teardown
   (when (or (stringp cl-store-test-file) (pathnamep cl-store-test-file))
     (ignore-errors (delete-file cl-store-test-file)))))

(addtest (test-cl-store+functions)
  cl-store-compatibility-test
  (with-backend 'cl-store+functions
    (let ((cl-store-tests::*test-file* *test-file*)
          (rt::*print-circle-on-failure* t)
          (rt::*expected-failures* '(cl-store-tests::custom.1 cl-store-tests::nocirc.1)))
      (format t "~&Initializing cl-store tests...~%")
      (prog1 (rt:do-tests)
        (format t "~&Finalized cl-store tests.~%")))))

(def-tester-for-standard-tests test-cl-store+functions ()
  (with-backend 'cl-store+functions
    (let ((function-set (get-current-function-set)))
      (run-prologue-test-code function-set)
      (store function-set *test-file*)
      (run-current-test function-set (restore *test-file*)))))
