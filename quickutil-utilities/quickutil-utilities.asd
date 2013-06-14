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
                             (:module alexandria
                              :components ((:file "definitions")
                                           (:file "binding")
                                           (:file "strings")
                                           (:file "conditions")
                                           ;; (:file "hash-tables")
                                           ;; (:file "io" :depends-on ("macros" "lists" "types"))
                                           ;; (:file "macros" :depends-on ("strings" "symbols"))
                                           ;; (:file "control-flow" :depends-on ("definitions" "macros"))
                                           ;; (:file "symbols" :depends-on ())
                                           ;; (:file "functions" :depends-on ("symbols" "macros"))
                                           ;; (:file "lists" :depends-on ("functions"))
                                           ;; (:file "types" :depends-on ("symbols" "lists"))
                                           ;; (:file "arrays" :depends-on ("types"))
                                           ;; (:file "sequences" :depends-on ("lists" "types"))
                                           ;; (:file "numbers" :depends-on ("sequences"))
                                           ;; (:file "features" :depends-on ("control-flow"))
                                           ))))))
