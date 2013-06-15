;;;; package.lisp
;;;; Copyright (c) 2012-2013 Robert Smith

(defpackage #:quickutil-client
  (:use #:cl)
  (:export #:set-quickutil-host
           #:enable-autoload-syntax
           #:utilize
           #:save-utils-as
           #:who-provides
           #:utilize-symbols
           #:utility-not-found-error))

(unless (find-package "QUICKUTIL")
  (defpackage #:quickutil
    (:nicknames #:qtl)
    (:use #:cl)
    (:import-from #:quickutil-client
                  #:set-quickutil-host
                  #:enable-autoload-syntax
                  #:utilize
                  #:save-utils-as
                  #:who-provides
                  #:utilize-symbols
                  #:utility-not-found-error)
    (:export #:set-quickutil-host
             #:enable-autoload-syntax
             #:utilize
             #:save-utils-as
             #:who-provides
             #:utilize-symbols
             #:utility-not-found-error)))

