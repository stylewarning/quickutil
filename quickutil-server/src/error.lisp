(in-package :cl-user)
(defpackage quickutil-server.error
  (:use :cl))
(in-package quickutil-server.error)

(cl-syntax:use-syntax :annot)

@export
(define-condition quickutil-server-error (simple-error) ())

@export
(define-condition quickutil-server-api-error (quickutil-server-error) ())
