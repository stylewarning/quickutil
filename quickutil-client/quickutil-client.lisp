;;;; quickutil-client.lisp

(in-package #:quickutil-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Internet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun download-url (url)
  (let* ((temp-stream (temporary-file:open-temporary))
         (temp (pathname temp-stream)))
    (close temp-stream)
    (nth-value 1 (ql-http:fetch url temp :quietly t))))

(defun load-from-url (url)
  (load (download-url url)))

(defun compile-and-load-from-url (url)
  (load (compile-file (download-url url))))

(defvar *quickutil-query-url*
  "http://qtl.clacklisp.org/api/emit-utility-code.lisp?utility=")

(defun quickutil-query-url (util-name)
  (concatenate 'string
               *quickutil-query-url*
               (string-downcase (if (symbolp util-name)
                                    (symbol-name util-name)
                                    util-name))))

(defun quickload (util-name)
  (compile-and-load-from-url (quickutil-query-url util-name)))