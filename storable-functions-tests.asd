;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2016 Jéssica Milaré
;;; See the file license for license information.

(defpackage :storable-functions-system
  (:use :cl :asdf))

(in-package :storable-functions-system)

(defsystem storable-functions-tests
  :name "storable-functions test"
  :maintainer "Jéssica Milaré"
  :author "Jéssica Milaré"
  :licence "MIT style"
  :description "Test system for Storable Functions."
  :depends-on (storable-functions lift cl-store+functions cl-store-tests)
  :components ((:module "tests"
                        :components ((:file "general")
                                     (:file "cl-store-tests" :depends-on ("general"))))))

(defmethod perform ((op test-op)
                    (system (eql (find-system :storable-functions-tests))))
  (funcall (find-symbol "RUN-ALL-TESTS" :storable-functions-tests)))
