;;;; autoload.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:quickutil-client)

(defparameter *autoload-lookup-suffix* "/api/reverse-lookup?symbol="
  "API call for autoloading.")

(defun autoload-lookup (symbol)
  (let* ((autoload-url (concatenate 'string *quickutil-host*
                                    *autoload-lookup-suffix*
                                    (symbol-name symbol)))
         (str (download-url-string autoload-url)))
    (if (string-equal "NIL" str)
        (error "Could not find originating utility for symbol: ~A"
               (symbol-name symbol))
        str)))

;;; XXX FIXME: This could use improved error handling.
(defun who-provides (symbol)
  "Which utility provides the symbol SYMBOL?"
  (assert (or (symbolp symbol)
              (stringp symbol)))
  (let ((who (ignore-errors (autoload-lookup (if (symbolp symbol)
                                                 symbol
                                                 (make-symbol symbol))))))
    (nth-value 0 (and who (intern who '#:keyword)))))

(defun |#?-reader| (stream subchar n)
  (declare (ignore subchar n))
  (let ((symbol (read stream t nil nil)))
    (unless (symbolp symbol)
      (error "#? requires a symbol to follow it."))
    ;; lookup the originating utility
    (let ((name (symbol-name symbol))
          (originating-util (autoload-lookup symbol)))
      (multiple-value-bind (found type)
          (find-symbol name '#:quickutil)
        (if (and found (eql type :external))
            found
            (progn
              (quickutil-client:quickload (intern originating-util '#:keyword))
              (find-symbol name '#:quickutil)))))))

(defun enable-autoload-syntax ()
  "Enable the use of #?SYMBOL, which automatically loads the symbol
SYMBOL from Quickutil."
  (set-dispatch-macro-character #\# #\? #'|#?-reader|))
