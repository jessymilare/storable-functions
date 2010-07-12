;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :storable-functions-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package '(:cl-store+functions :cl-store)))

(defun run-cl-store+functions-tests (&rest args)
  (apply #'run-tests :suite 'test-cl-store+functions args))

(deftestsuite test-cl-store+functions ()
  ((cl-store-test-file #P".cl-store+functions-test-file")
   (cl-store-backend (find-backend 'cl-store+functions)))
  (:teardown
   (when (or (stringp cl-store-test-file) (pathnamep cl-store-test-file))
     (ignore-errors (delete-file cl-store-test-file)))))

(addtest (test-cl-store+functions)
  cl-store-compatibility-test
  (with-backend cl-store-backend
    (let ((cl-store-tests::*test-file* cl-store-test-file)
	  (rt::*print-circle-on-failure* t)
	  (rt::*expected-failures* '(cl-store-tests::custom.1 cl-store-tests::nocirc.1)))
      (format t "~&Initializing cl-store tests...~%")
      (prog1 (rt:do-tests)
	(format t "~&Finalized cl-store tests.~%")))))

(def-tester-for-standard-tests test-cl-store+functions ()
  (with-backend cl-store-backend
    (let ((function-set (get-current-function-set)))
      (run-prologue-test-code function-set)
      (store function-set cl-store-test-file)
      (run-current-test function-set (restore cl-store-test-file)))))
