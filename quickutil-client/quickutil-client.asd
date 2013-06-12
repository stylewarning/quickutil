;;;; quickutil-client.asd
;;;; Copyright (c) 2013 Robert Smith

(asdf:defsystem #:quickutil-client
  :serial t
  :description "The Quickutil client for downloading utility code."
  :author "Robert Smith <quad@symbo1ics.com>"
  :components ((:file "package")
               (:file "quickutil-client"))
  :depends-on ("drakma" "temporary-file"))
