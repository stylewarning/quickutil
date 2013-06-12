;;;; autoload.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:quickutil-client)

(defun |#?-reader| (stream subchar n)
  (declare (ignore subchar n))
  (let ((symbol (read stream t nil nil)))
    (unless (symbolp symbol)
      (error "#? requires a symbol to follow it."))
    (let* ((name (symbol-name symbol)))
      (multiple-value-bind (found type)
          (find-symbol name '#:quickutil)
        (if (and found (eql type :external))
            found
            (progn
              (quickutil-client:quickload (intern name '#:keyword))
              (find-symbol name '#:quickutil)))))))

(defun enable-autoload-syntax ()
  "Enable the use of #?SYMBOL, which automatically loads the symbol
SYMBOL from Quickutil."
  (set-dispatch-macro-character #\# #\? #'|#?-reader|))
