;;;; quickutil-client.lisp
;;;; Copyright (c) 2012-2013 Robert Smith

(in-package #:quickutil-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Internet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XXX FIXME: Make error handling a little more robust.
(defun download-url (url)
  "Download data from the URL URL and put it in a temporary
file. Return the pathname of the temporary file."
  (let* ((temp-stream (temporary-file:open-temporary))
         (temp (pathname temp-stream)))
    (unwind-protect (write-string (nth-value 0 (drakma:http-request url))
                                  temp-stream)
      (close temp-stream))
    temp))

(defun load-from-url (url)
  "Load into the Lisp image the data from the URL URL."
  (load (download-url url)))

(defun compile-and-load-from-url (url)
  "Compile and load into the Lisp image the data from the URL URL."
  (load (compile-file (download-url url))))

(defvar *quickutil-query-url*
  "http://www.quickutil.org"
  "The host from which Quickutil downloads.")

(defvar *quickutil-query-suffix*
  "/api/emit-utility-code.lisp?utility="
  "The API string used to query a utility.")

(defun set-quickutil-host (hostname)
  "Set the host from which Quickutil downloads from to
HOSTNAME. HOSTNAME must be an HTTP host."
  (let ((pos (search "http://" hostname :test #'char=)))
    (if (and pos (zerop pos))
        (setf *quickutil-query-url* hostname)
        (setf *quickutil-query-url* (concatenate 'string "http://" hostname)))))

(defun quickutil-query-url (util-name)
  "Construct the URL from which to query for the utility named
UTIL-NAME."
  (concatenate 'string
               *quickutil-query-url*
               *quickutil-query-suffix*
               (string-downcase (if (symbolp util-name)
                                    (symbol-name util-name)
                                    util-name))))

(defun quickload (util-name)
  "Load the utility UTIL-NAME and its dependencies."
  (compile-and-load-from-url (quickutil-query-url util-name)))
