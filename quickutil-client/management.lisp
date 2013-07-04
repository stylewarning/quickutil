;;;; management.lisp
;;;; Copyright (c) 2013 Robert Smith

(defpackage #:quickutil-client-management
  (:use #:cl #:asdf)
  (:export #:load-quickutil-utilities
           #:unload-quickutil-utilities
           #:with-quickutil-utilities
           #:*verbose*))

(in-package #:quickutil-client-management)

(defvar *verbose* nil
  "Dictates whether loading should be verbose.")

(defun unbind-symbol (symbol)
  "Unbind the symbol denoting a variable, function, or macro."
  (cond
    ((boundp symbol) (makunbound symbol))
    ((fboundp symbol) (unless (special-operator-p symbol)
                        (fmakunbound symbol)))
    (t nil)))

(defun clean-and-delete-package (package-designator)
  "Clean up the package designated by PACKAGE-DESIGNATOR (unbind all
of the bound symbols), and delete the package, if it exists."
  (when (or (packagep package-designator)
            (find-package package-designator))
    (let ((package (if (packagep package-designator)
                       package-designator
                       (find-package package-designator))))
      ;; Clean up all the symbols.
      (do-symbols (sym package-designator)
        (when (eq (symbol-package sym) package)
          (unbind-symbol sym)))
      
      ;; Delete the package.
      (delete-package package-designator))))

(defun load-quickutil-utilities (&key (verbose *verbose*))
  (when verbose
    (format t "~&;;; Loading Quickutil utilities...~%"))
  (let ((*standard-output* (make-broadcast-stream)))
    (operate 'load-op :quickutil-utilities :force t :verbose nil)))

(defun unload-quickutil-utilities (&key (verbose *verbose*))
  (when verbose
    (format t "~&;;; Clearing QUICKUTIL-UTILITIES system...~%"))
  (asdf:clear-system :quickutil-utilities)
  
  (when verbose
    (format t "~&;;; Unloading QUICKUTIL-UTILITIES.UTILITIES...~%"))
  (clean-and-delete-package '#:quickutil-utilities.utilities)
  
  (when verbose
    (format t "~&;;; Unloading QUICKUTIL-UTILITIES...~%"))
  (clean-and-delete-package '#:quickutil-utilities)
  
  (when verbose
    (format t "~&;;; Collecting trash...~%"))
  (trivial-garbage:gc :full t :verbose verbose))

(defmacro with-quickutil-utilities (&body body)
  "Load Quickutil utilities, execute BODY, and unload them, returning
the last result of BODY."
  `(unwind-protect (progn
                     (quickutil-client-management:load-quickutil-utilities)
                     ,@body)
     (quickutil-client-management:unload-quickutil-utilities)))

