;;;; quickutil-utilities.asd

(asdf:defsystem #:quickutil-utilities
  :serial t
  :description "The utilities and backend to Quickutil."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "See LICENSE file."
  :components (
               ;; License: BSD 3-clause
               (:file "package")
               (:file "queue")
               (:file "defutil")
               
               ;; License: None. (Public Domain)
               (:module utilities
                :serial t
                :components ((:file "arithmetic")
                             (:file "trigonometry")
                             (:file "lists")
                             (:file "combinators")
                             (:file "arrays")
                             (:file "instrumentation")
                             (:file "trees")
                             (:file "symbols")
                             (:file "language")
                             (:file "constants")
                             (:file "control")
                             (:file "hash-tables")
                             (:file "random")
                             (:file "generic")
                             (:file "conses")))))
