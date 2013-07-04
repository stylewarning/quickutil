;;;; quickutil-client-management.asd
;;;; Copyright (c) 2013 Robert Smith

(asdf:defsystem #:quickutil-client-management
  :serial t
  :description "The Quickutil client manager."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause. See LICENSE file."
  :components ((:file "management"))
  :depends-on ("trivial-garbage"))
