;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2016 Jéssica Milaré
;;; See the file license for license information.

(defpackage :storable-functions-system
  (:use :cl :asdf))

(in-package :storable-functions-system)

(defsystem-connection cl-store+functions
  :name "cl-store+functions"
  :version "0.0.2"
  :maintainer "Jéssica Milaré"
  :author "Jéssica Milaré"
  :licence "MIT style"
  :description "Includes the ability to store function objects to cl-store."
  :requires (storable-functions cl-store)
  :components ((:module "cl-store+functions"
                        :components ((:file "cl-store+functions")))))

(defmethod perform ((op test-op)
                    (system (eql (find-system :cl-store+functions))))
  (oos 'load-op :cl-store+functions-tests)
  (oos 'test-op :cl-store+functions-tests))
