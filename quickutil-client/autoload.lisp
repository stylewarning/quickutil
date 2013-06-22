;;;; autoload.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:quickutil-client)

;;;; Syntax for automatically loading symbols.

(defun |#?-reader| (stream subchar n)
  (declare (ignore subchar n))
  (let ((symbol (read stream t nil nil)))
    (unless (symbolp symbol)
      (error "#? requires a symbol to follow it."))
    ;; lookup the originating utility
    (let ((name (symbol-name symbol))
          (originating-util (who-provides symbol)))
      (multiple-value-bind (found type)
          (find-symbol name '#:quickutil)
        (if (and found (eql type :external))
            found
            (progn
              (quickutil-client:utilize (intern originating-util '#:keyword))
              (find-symbol name '#:quickutil)))))))

(defun enable-autoload-syntax ()
  "Enable the use of #?SYMBOL, which automatically loads the symbol
SYMBOL from Quickutil."
  (set-dispatch-macro-character #\# #\? #'|#?-reader|))
