;;;; quickutil-utilities-test.asd

(asdf:defsystem #:quickutil-utilities-test
 :depends-on ("quickutil-server"
              "quickutil-client")
 :components ((:file "test-utilities")))
