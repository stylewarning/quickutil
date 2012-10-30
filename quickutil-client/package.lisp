;;;; package.lisp

(defpackage #:quickutil-client
  (:use #:cl))

(unless (find-package #:quickutil)
  (defpackage #:quickutil
    (:use #:cl)))

