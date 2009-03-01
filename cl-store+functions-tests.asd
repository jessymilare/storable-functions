;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :storable-functions-system
  (:use :cl :asdf))

(in-package :storable-functions-system)

(defsystem cl-store+functions-tests
  :name "cl-store+functions test"
  :maintainer "Gustavo Henrique Milaré"
  :author "Gustavo Henrique Milaré"
  :licence "MIT style"
  :description "Test system for cl-store+functions"
  :depends-on (storable-functions-tests cl-store+functions cl-store-tests)
  :components ((:module "tests"
		:components ((:file "cl-store-tests")))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-store+functions-tests))))
  (funcall (find-symbol "RUN-CL-STORE+FUNCTIONS-TESTS" :storable-functions-tests)))
