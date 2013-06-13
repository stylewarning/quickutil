;;;; quickutil-client.lisp
;;;; Copyright (c) 2012-2013 Robert Smith

(in-package #:quickutil-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Internet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun download-url-string (url)
  "Download the URL URL to a string."
  (nth-value 0 (drakma:http-request url)))

;; XXX FIXME: Make error handling a little more robust.
(defun download-url (url)
  "Download data from the URL URL and put it in a temporary
file. Return the pathname of the temporary file."
  (let* ((temp-stream (temporary-file:open-temporary))
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

(defvar *quickutil-query-suffix*
  "/api/emit?utility="
  "The API string used to query a utility.")

(defun quickutil-query-url (util-name)
  "Construct the URL from which to query for the utility named
UTIL-NAME."
  (concatenate 'string
               *quickutil-host*
               *quickutil-query-suffix*
               (string-downcase (if (symbolp util-name)
                                    (symbol-name util-name)
                                    util-name))))

;;; XXX FIXME: Right now this naively loads the utilities, causing
;;; many repeat compilations.
;;;
;;; XXX FIXME: Error when utility is not found instead of just trying
;;; to compile NIL.
(defun quickload (&rest util-names)
  "Load the utilities UTIL-NAMES and their dependencies."
  (dolist (util-name util-names)
    (compile-and-load-from-url (quickutil-query-url util-name))))
