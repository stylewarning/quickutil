;;;; package.lisp
;;;; Copyright (c) 2012 Robert Smith

(defpackage #:quickutil-utilities
  (:use #:cl)
  (:nicknames #:qtl-utl)
  (:export
   #:defutil
   #:all-utilities
   #:all-categories
   #:utils-in-category
   #:emit-utility-code
   #:pretty-print-utility-code))


;;; All actual utilities get shoved into this package.
(defpackage #:quickutil-utilities.utilities
  (:use #:cl #:quickutil-utilities)
  (:nicknames #:qtl-utl.utl))
