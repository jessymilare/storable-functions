;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2016 Jéssica Milaré
;;; See the file license for license information.

(in-package :storable-functions)

(deflex lexical-environment nil)

(macrolet
    ((def (name type)
       `(defmacro ,name (vars &body body)
          (let ((variables (mapcar #'ensure-car vars)))
            (multiple-value-bind (body declarations) (parse-body body)
              ()
              `(,',type ,,'vars
                 (let ((lexical-environment
                        (make-instance
                         'let-closure-info :type 'let*
                                        ; no need for let since values will
                                        ; already be known by restorage time
                         :environment lexical-environment
                         :variables ',,'variables
                         :values-accessor
                         ,(generate-closure-values-accessor variables)
                         :declarations
                         ',,'(mappend #'cdr declarations))))
                   ,@,'declarations
                   ,@,'body)))))))
  (def st-let let)
  (def st-let* let*))

(defun parse-function (class lambda-list body
                       &optional name (get-environment t))
  `(make-instance ',class
                  :lambda-list ',lambda-list :body ',body
                  ,@(if get-environment
                        `(:environment lexical-environment))
                  ,@(if name `(:name ',name))))

(defmacro st-lambda (lambda-list &body body)
  (with-unique-names (function)
    `(let ((,function (lambda ,lambda-list ,@body)))
       (setf (get-function-info ,function)
             ,(parse-function 'lambda-info lambda-list body))
       ,function)))

(defmacro st-named-lambda (name lambda-list &body body)
  (with-unique-names (function)
    `(let ((,function (named-lambda ,lambda-list ,@body)))
       (setf (get-function-info ,function)
             ,(parse-function 'named-lambda-info lambda-list body name))
       ,function)))

(macrolet
    ((def (name type)
       `(defmacro ,name (fspecs &body body)
          (let* ((func-names (mapcar #'first fspecs))
                 (info-names (mapcar #'(lambda (func-name)
                                         (gensym (symbol-name func-name)))
                                     func-names))
                 (infos (mapcar
                         (curry #'apply
                                #'(lambda (name lambda-list &rest func-body)
                                    (parse-function
                                     'named-lambda-info lambda-list
                                     func-body name ,(eq type 'flet))))
                         fspecs)))
            (multiple-value-bind (body declarations) (parse-body body)
              `(let ,(mapcar #'list info-names infos)
                 (,',type ,fspecs
                          (let ((lexical-environment
                                 (make-instance
                                  'flet-closure-info :type ',',type
                                  :environment lexical-environment
                                  :functions (list ,@info-names)
                                  :declarations
                                  ',,'(mappend #'cdr declarations))))
                            ;; Label functions may depend on the entire
                            ;; labels form.
                            ;; Flet functions, on the other hand, don't.
                            ,@,(when (eq type 'labels)
                                     `(mapcar #'(lambda (info)
                                                  `(setf (info-environment ,info)
                                                         lexical-environment))
                                              info-names))
                            ,@(mapcar #'(lambda (func-name info-name)
                                          `(setf (get-function-info
                                                  (function ,func-name))
                                                 ,info-name))
                                      func-names info-names)
                            ,@,'declarations
                            ,@,'body))))))))
  (def st-flet flet)
  (def st-labels labels))

(macrolet ((def (name type)
             `(defmacro ,name (macros &body body)
                (multiple-value-bind (body declarations) (parse-body body)
                  `(,',type ,macros
                            (let ((lexical-environment
                                   (make-instance
                                    'macro-closure-info :type ',',type
                                    :environment lexical-environment
                                    :macros ',macros
                                    :declarations
                                    ',,'(mappend #'cdr declarations))))
                              ,@,'declarations
                              ,@,'body))))))
  (def st-macrolet macrolet)
  (def st-symbol-macrolet symbol-macrolet))

(defmacro st ((function-name &rest arguments))
  (case function-name
    (function
     (assert (not (cdr arguments)))
     (let ((arg (first arguments)))
       (if (and (consp arg)
                (member (car arg) '(lambda named-lambda)))
           `(st ,arg)
           ;; Either this is a global function (e.g. (function sin))
           ;; (which we do not handle - cl-store or any persistence
           ;; library can easily handle this) or a local function
           ;; (which should already have been handled by st-flet
           ;; or st-lambda macro)
           ;; either way, there is nothing left to do here.
           `(function ,arg))))
    ((lambda named-lambda let let* flet labels macrolet symbol-macrolet)
     `(,(find-symbol (concatenate 'string "ST-" (symbol-name function-name)))
        ,@arguments))
    (t (let ((new-args (loop repeat (length arguments)
                            collect (gensym))))
         (with-unique-names (function)
           `(let* (,@(mapcar #'list new-args arguments)
                   (,function (,function-name . ,new-args)))
              (when (functionp ,function)
                (setf (get-function-info ,function)
                      (make-instance 'function-call-info
                                     :function-name ',function-name
                                     :values (list . ,new-args))))
              ,function))))))

(defmacro stq (form)
  (with-unique-names (function)
    `(let ((,function ,form))
       (when (functionp ,function)
         (setf (get-function-info ,function)
               (make-instance 'quoted-function-info
                              :body ',form)))
       ,function)))
