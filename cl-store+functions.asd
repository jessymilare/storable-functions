;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file licence for licence information.

(defpackage :storable-functions-system
  (:use :cl :asdf))

(in-package :storable-functions-system)

(defsystem cl-store+functions
  :name "cl-store+functions"
  :version "0.0.1"
  :maintainer "Gustavo Henrique Milaré"
  :author "Gustavo Henrique Milaré"
  :licence "MIT style"
  :description "Includes the ability to store function objects to cl-store."
  :depends-on (storable-functions cl-store)
  :components ((:module "cl-store+functions"
		:serial t
		:components ((:file "cl-store+functions")))))
