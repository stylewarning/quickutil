;;;; quickutil.asd
;;;; Copyright (c) 2013 Robert Smith and Eitarow Fukamachi

;;;; This is the public Quickutil system.

(asdf:defsystem #:quickutil
  :description "The Quickutil client."
  :author "Robert Smith <quad@symbo1ics.com>, Eitarow Fukamachi <e.arrows@gmail.com>"
  :depends-on ("quickutil-client"))
