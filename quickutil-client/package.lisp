;;;; package.lisp
;;;; Copyright (c) 2012-2013 Robert Smith

(defpackage #:quickutil-client
  (:documentation "Contains client-side code for acquiring, compiling,
  loading, and saving utilties.")
  (:nicknames #:qtlc)
  (:use #:cl)
  (:export #:set-quickutil-host
           #:enable-autoload-syntax
           #:utilize
           #:utilize-categories
           #:category-utilities
           #:save-utils-as
           #:who-provides
           #:utilize-symbols
           #:utility-not-found-error))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:quickutil)
    (defpackage #:quickutil
      (:documentation "Package that contains the actual utility functions.")
      (:nicknames #:qtl)
      (:use #:cl))))

