;;;; quickutil-utilities.asd

(asdf:defsystem #:quickutil-utilities
  :serial t
  :description "The utilities and backend to Quickutil."
  :author "Robert Smith <quad@symbo1ics.com>"
  ;; :license "???"
  :components ((:file "package")
               (:file "queue")
               (:file "defutil")))

