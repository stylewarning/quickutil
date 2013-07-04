;;;; management.lisp
;;;; Copyright (c) 2013 Robert Smith

(defpackage #:quickutil-client-management
  (:use #:cl #:asdf)
  (:export #:load-quickutil-utilities
           #:unload-quickutil-utilities
           #:with-quickutil-utilities))

(in-package #:quickutil-client-management)

(defun unbind-symbol (symbol)
  (cond
    ((boundp symbol) (makunbound symbol))
    ((fboundp symbol) (unless (special-operator-p symbol)
                        (fmakunbound symbol)))
    (t nil)))

(defun clean-and-delete-package (package-designator)
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

(defun load-quickutil-utilities (&key (verbose t))
  (when verbose
    (format t "~&;;; Loading Quickutil utilities...~%"))
  (let ((*standard-output* (make-broadcast-stream)))
    (operate 'load-op :quickutil-utilities :force t :verbose nil)))

(defun unload-quickutil-utilities (&key (verbose t))
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
  `(unwind-protect (progn
                     (quickutil-client-management:load-quickutil-utilities)
                     ,@body)
     (quickutil-client-management:unload-quickutil-utilities)))

#+(or)
(defmethod operate :after ((o load-op)
                           (s (eql (find-system :quickutil-client)))
                           &key &allow-other-keys)
  (break "STOPPED")
  (unload-quickutil-utilities))
