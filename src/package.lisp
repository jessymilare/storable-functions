;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :storable-functions
  (:use :trivial-garbage :cl :metatilities)
  (:nicknames :st-fun)
  (:export #:code-information

	   #:with-storable-functions-restorage #:with-storable-functions-storage
	   #:store-code-info #:restore-code-info #:get-function-referrer

	   #:get-function-info #:rem-function-info

	   #:st-let #:st-let*
	   #:st-lambda #:st-named-lambda
	   #:st-flet #:st-labels
	   #:st-macrolet #:st-symbol-macrolet
	   #:st #:stq))
