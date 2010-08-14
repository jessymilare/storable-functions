;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :storable-functions-system
  (:use :cl :asdf))

(in-package :storable-functions-system)

(defsystem storable-functions
  :name "Storable Functions"
  :version "0.0.3"
  :maintainer "Gustavo Henrique Milaré"
  :author "Gustavo Henrique Milaré"
  :licence "MIT style"
  :description "Implements a way to transform functions from and to CLOS
instances of some specific classes. It contains a set of macros for making
this transformation possible, and tools for actually doing the transformation.
The goal is to provide a simple, portable way to serialize functions. Portable
in the sense that it should work across multiple Common Lisp implementations.
Simple in the sense that supporting the serialization of all functions defined
using this library should be just a matter of defining / redefining one or two
methods of the serialization protocol.
The only restriction to make functions serializable is to use a set of macros
to create closure and function information."
  :depends-on (trivial-garbage alexandria asdf-system-connections)
  :components ((:module "src"
		:components ((:file "package")
			     (:file "utils" :depends-on ("package"))
			     (:file "classes" :depends-on ("utils"))
			     (:file "storage" :depends-on ("classes"))
			     (:file "deflex" :depends-on ("package"))
			     (:file "macros" :depends-on ("deflex" "classes"))))))

(defmethod perform ((op test-op) (system (eql (find-system :storable-functions))))
  (oos 'load-op :storable-functions-tests)
  (oos 'test-op :storable-functions-tests))
