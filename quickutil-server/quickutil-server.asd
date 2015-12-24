(in-package :cl-user)
(defpackage quickutil-server-asd
  (:use :cl :asdf))
(in-package :quickutil-server-asd)

(defsystem quickutil-server
  :version "0.1"
  :description "Implementation of the Quickutil server."
  :author "Eitarow Fukamachi <e.arrows@gmail.com>"
  :license "BSD 3-clause. See LICENSE file."
  :depends-on (:ningle
               :clack-middleware-csrf
               :cl-syntax
               :cl-syntax-annot
               :cl-ppcre
               :yason
               :quickutil-utilities
               :closure-template
               :cl-fad
               :cl-markdown
               :dbi
               :multival-plist
               :trivial-shell)
  :components ((:module "src"
                :components
                ((:file "app")
                 (:file "core" :depends-on ("app" "db" "constants"))
                 (:file "constants")
                 (:file "db" :depends-on ("constants"))
                 (:module "controller"
                  :depends-on ("app" "core" "error")
                  :components
                  ((:file "web")
                   (:file "api")))
                 (:file "error")))))
