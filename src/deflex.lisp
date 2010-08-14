;;;; deflex.lisp -- Define "global lexical variables" in Common Lisp.

;;; Copyright 2003-2007 Rob Warnock <rpw3@rpw3.org>. All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; [Extracted from my personal "utils.lisp" (available someday, someday...)]

;;; DEFLEX -- Define "global lexical variables", that is, top-level
;;; variables (convenient when debugging) that are lexical in scope,
;;; and thus don't pollute either the special or lexical variable spaces
;;; [except for the names of the "shadow" variables (c.f.), which are
;;; hopefully non-conflicting in most cases]. Thanks to the denizens
;;; of the "comp.lang.lisp" newsgroup for many useful discussions (and
;;; flames!) on this topic, and for the suggestion for the simple and
;;; efficient (albeit inelegant) "shadow" variable approach used here.
;;; [Note: Like several others, I had previously used a single global
;;; adjustable vector of shadow values, with complicated compile-time
;;; allocation of indices so that symbol-macro FOO expanded into something
;;; like this: (AREF *LEXICAL-STORE* (LOAD-TIME-VALUE {index-for-FOO})).
;;; But the following approach is much simpler and more maintainable.]
;;;
;;; 2005-06-12 -- Package bugfix thanks to Adam Warner <adam@consulting.net.nz>
;;;

(in-package :storable-functions)

(defmacro deflex (var val &optional (doc nil docp))    
  "Define a top level (global) lexical VAR with initial value VAL,
  which is assigned unconditionally as with DEFPARAMETER. If a DOC
  string is provided, it is attached to both the name |VAR| and the
  name *STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of
  kind 'VARIABLE. The new VAR will have lexical scope and thus may be
  shadowed by LET bindings without affecting its dynamic (global) value."
  (let* ((s0 (symbol-name '#:*storage-for-deflex-var-))
         (s1 (symbol-name var))
         (s2 (symbol-name '#:*))
         (s3 (symbol-package var))      ; BUGFIX [see above]
         (backing-var (intern (concatenate 'string s0 s1 s2) s3)))
    ;; Note: The DEFINE-SYMBOL-MACRO must be the last thing we do so
    ;; that the value of the form is the symbol VAR.
    (if docp
      `(progn
         (defparameter ,backing-var ,val ,doc)
         (setf (documentation ',var 'variable) ,doc)
         (define-symbol-macro ,var ,backing-var))
      `(progn
         (defparameter ,backing-var ,val)
         (define-symbol-macro ,var ,backing-var)))))

;;; File downloaded from http://rpw3.org/hacks/lisp/deflex.lisp
