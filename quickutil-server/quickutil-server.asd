(in-package :cl-user)
(defpackage quickutil-server-asd
  (:use :cl :asdf))
(in-package :quickutil-server-asd)

(defsystem quickutil-server
  :version "0.1"
  :author "Eitarow Fukamachi"
  :depends-on (:ningle
               :cl-syntax
               :cl-syntax-annot
               :cl-ppcre
               :yason
               :quickutil-utilities
               :cl-emb)
  :components ((:module "src"
                :components
                ((:file "app")
                 (:file "core" :depends-on ("app"))
                 (:file "controller" :depends-on ("app" "error"))
                 (:file "error")))))
