(in-package :cl-user)
(defpackage quickutil-server.constants
  (:use :cl)
  (:import-from :fad
                :file-exists-p))
(in-package :quickutil-server.constants)

(cl-syntax:use-syntax :annot)

@export
(defparameter *config* nil)

@export
(defparameter *db* nil)

@export
(defparameter *static-files* nil)

(defun slurp-file (path)
  "Read a specified file and return the content as a sequence."
  (with-open-file (stream path :direction :input)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

@export
(defun load-config ()
  (let ((config-file
         (merge-pathnames "src/config.lisp"
                          (asdf:component-pathname (asdf:find-system :quickutil-server)))))
    (when (file-exists-p config-file)
      (setf *config*
            (eval
             (read-from-string
              (slurp-file config-file)))))))
