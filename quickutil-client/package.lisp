;;;; package.lisp
;;;; Copyright (c) 2012-2013 Robert Smith

(defpackage #:quickutil-client
  (:documentation "Contains client-side code for acquiring, compiling,
  loading, and saving utilties.")
  (:nicknames #:qtlc)
  (:use #:cl)
  (:export #:enable-autoload-syntax
           #:utilize
           #:utilize-utilities
           #:utilize-categories
           #:utilize-symbols
           #:save-utils-as
           #:who-provides
           #:utility-not-found-error))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:quickutil)
    (defpackage #:quickutil
      (:documentation "Package that contains the actual utility functions.")
      (:nicknames #:qtl)
      (:use #:cl)
      (:export #:*utilities*))))

