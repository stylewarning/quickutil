;;;; http.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:quickutil-client)

;;;; Functions for downloading data off of the Internet.

(defun download-url-string (url)
  "Download the URL URL to a string."
  (nth-value 0 (drakma:http-request url)))

;; XXX FIXME: Make error handling a little more robust.
(defun download-url (url)
  "Download data from the URL URL and put it in a temporary
file. Return the pathname of the temporary file."
  (let* ((temp-stream (cl-fad:open-temporary))
         (temp (pathname temp-stream)))
    (unwind-protect (write-string (download-url-string url)
                                  temp-stream)
      (close temp-stream))
    temp))

(defun load-from-url (url)
  "Load into the Lisp image the data from the URL URL."
  (load (download-url url)))

(defun compile-and-load-from-url (url)
  "Compile and load into the Lisp image the data from the URL URL."
  (load (compile-file (download-url url))))
