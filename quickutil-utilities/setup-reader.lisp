;;;; setup-reader.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:quickutil-utilities)

;;; Set up the reader so heredocs can be used for the definition of
;;; utilities.
(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)
