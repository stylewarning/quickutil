;;;; quickutil-utilities.asd

(asdf:defsystem #:quickutil-utilities
  :serial t
  :description "The utilities and backend to Quickutil."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "See LICENSE file."
  :components (
               ;; License: BSD 3-clause
               (:static-file "LICENSE")
               (:file "package")
               (:file "queue")
               (:file "defutil")
               
               ;; License: None. (Public Domain)
               (:module utilities
                :serial t
                :components ((:static-file "LICENSE")
                             (:file "arithmetic")
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
                             (:file "conses")
                             (:file "synonyms")
                             (:module split-sequence
                              :serial t
                              :components ((:file "split-sequence")))
                             (:module alexandria
                              :serial t
                              :components ((:file "definitions")
                                           (:file "binding")
                                           (:file "strings")
                                           (:file "conditions")
                                           (:file "hash-tables")
                                           (:file "symbols")
                                           (:file "macros")
                                           (:file "control-flow")
                                           (:file "features")
                                           (:file "functions")
                                           (:file "types")
                                           ;; (:file "io" :depends-on ("macros" "lists" "types"))
                                           ;; (:file "lists" :depends-on ("functions"))
                                           ;; (:file "arrays" :depends-on ("types"))
                                           ;; (:file "sequences" :depends-on ("lists" "types"))
                                           ;; (:file "numbers" :depends-on ("sequences"))
                                           ))))))
