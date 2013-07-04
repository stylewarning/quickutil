;;;; management.lisp
;;;; Copyright (c) 2013 Robert Smith

(defpackage #:quickutil-client-management
  (:use #:cl #:asdf)
  (:export #:load-quickutil-utilities
           #:unload-quickutil-utilities
           #:with-quickutil-utilities))

(in-package #:quickutil-client-management)

(defun delete-package-if-exists (package-designator)
  (when (or (packagep package-designator)
            (find-package package-designator))
    (delete-package package-designator)))

(defun load-quickutil-utilities (&key (verbose t))
  (when verbose
    (format t "~&;;; Loading Quickutil utilities...~%"))
  (let ((*standard-output* (make-broadcast-stream)))
    (operate 'load-op :quickutil-utilities :force t :verbose nil)))

(defun unload-quickutil-utilities (&key (verbose t))
  (when verbose
    (format t "~&;;; Unloading QUICKUTIL-UTILITIES.UTILITIES...~%"))
  (delete-package-if-exists '#:quickutil-utilities.utilities)
  
  (when verbose
    (format t "~&;;; Unloading QUICKUTIL-UTILITIES...~%"))
  (delete-package-if-exists '#:quickutil-utilities)
  
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
