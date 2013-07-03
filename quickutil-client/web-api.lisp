;;;; web-api.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:quickutil-client)

;;;; Functions for building Quickutil API requests.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Host ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;; Utility Code Request ;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *quickutil-query-suffix*
  "/api/emit?"
  "The API string used to query a utility.")

(defvar *quickutil-query-argument*
  "utility=")

(defun quickutil-query-url (util-names)
  "Construct the URL from which to query for the utilities named
UTIL-NAMES."
  (assert util-names (util-names) "UTIL-NAMES must be non-null.")
  (flet ((format-argument (util-name &optional (ampersand? t))
           (when ampersand?
             (write-string "&"))
           (write-string *quickutil-query-argument*)
           (write-string (string-downcase (if (symbolp util-name)
                                              (symbol-name util-name)
                                              util-name)))))
    (with-output-to-string (*standard-output*)
      (write-string *quickutil-host*)
      (write-string *quickutil-query-suffix*)
      (format-argument (first util-names) nil)
      (mapc #'format-argument (rest util-names)))))

;;;;;;;;;;;;;;;;;;;;;; Category Symbols Request ;;;;;;;;;;;;;;;;;;;;;;

(defparameter *category-lookup-suffix* "/api/list/"
  "API call for finding category symbols.")

(defun category-url (category-name)
  "Construct the url for querying the symbols in the category named
CATEGORY-NAME."
  (concatenate 'string
               *quickutil-host*
               *category-lookup-suffix*
               (symbol-name category-name)))


;;;;;;;;;;;;;;;;;;;;;;; Reverse Symbol Lookup ;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *reverse-lookup-suffix* "/api/reverse-lookup?symbol="
  "API call for autoloading.")

(defun reverse-lookup-url (symbol)
  "Construct the url for doing a reverse lookup on the symbol SYMBOL."
  (concatenate 'string
               *quickutil-host*
               *reverse-lookup-suffix*
               (symbol-name symbol)))
