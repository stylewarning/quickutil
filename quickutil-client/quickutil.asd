;;;; quickutil.asd
;;;; Copyright (c) 2013 Robert Smith and Eitarow Fukamachi

;;;; This is the public Quickutil system.

(asdf:defsystem #:quickutil
  :description "The Quickutil client."
  :author "Robert Smith <quad@symbo1ics.com>, Eitarow Fukamachi <e.arrows@gmail.com>"
  :license "BSD 3-clause. See LICENSE file."
  :depends-on ("quickutil-client")
  :serial t
  :components ((:file "clean-up-quickutil")))
