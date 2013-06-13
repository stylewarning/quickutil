;;;; host.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:quickutil-client)

(defvar *quickutil-host*
  "http://www.quickutil.org"
  "The host from which Quickutil downloads.")

(defun set-quickutil-host (hostname)
  "Set the host from which Quickutil downloads from to
HOSTNAME. HOSTNAME must be an HTTP host."
  (let ((pos (search "http://" hostname :test #'char=)))
    (if (and pos (zerop pos))
        (setf *quickutil-host* hostname)
        (setf *quickutil-host* (concatenate 'string "http://" hostname)))))
