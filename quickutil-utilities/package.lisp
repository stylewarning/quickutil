;;;; package.lisp
;;;; Copyright (c) 2012 Robert Smith

(defpackage #:quickutil-utilities
  (:use #:cl)
  (:nicknames #:qtl-utl)
  (:export #:defutil))


;;; All actual utilities get shoved into this package.
(defpackage #:quickutil
  (:use #:cl #:quickutil-utilities)
  (:nicknames #:qtl))