;;;; http.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:quickutil-client)

;;;; Functions for downloading data off of the Internet.

(defun download-url-string (url)
  "Download the URL URL to a string."
  (nth-value 0 (drakma:http-request url)))

(defmacro with-temp-file (stream-var file-var &body body)
  `(let* ((,stream-var (cl-fad:open-temporary))
          (,file-var (pathname ,stream-var)))
     (unwind-protect (progn ,@body)
       (close ,stream-var))
     ,file-var))

;; XXX FIXME: Make error handling a little more robust.
(defun download-url (url)
  "Download data from the URL URL and put it in a temporary
file. Return the pathname of the temporary file."
  (with-temp-file stream file
    (write-string (download-url-string url) stream)))

(defun load-from-url (url)
  "Load into the Lisp image the data from the URL URL."
  (load (download-url url)))

(defun compile-and-load-from-url (url)
  "Compile and load into the Lisp image the data from the URL URL."
  (load (compile-file (download-url url))))
