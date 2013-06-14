;;;; host.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:quickutil-client)

(defvar *quickutil-host*
  "http://quickutil.org"
  "The host from which Quickutil downloads.")

(defun set-quickutil-host (hostname)
  "Set the host from which Quickutil downloads from to
HOSTNAME. HOSTNAME must be an HTTP host."
  (warn "Changing Quickutil host to: ~A" hostname)
  (let ((pos (search "http://" hostname :test #'char=)))
    (if (and pos (zerop pos))
        (setf *quickutil-host* hostname)
        (setf *quickutil-host* (concatenate 'string "http://" hostname)))))
