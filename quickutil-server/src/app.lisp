(in-package :cl-user)
(defpackage quickutil-server.app
  (:use :cl)
  (:import-from :lack.component
                :call)
  (:import-from :ppcre
                :scan)
  (:import-from :ningle
                :<app>))
(in-package :quickutil-server.app)

(cl-syntax:use-syntax :annot)

@export
(defvar *web* (make-instance 'ningle:<app>))

@export
(defvar *api* (make-instance 'ningle:<app>))

@export
(defvar *app*
    #'(lambda (env)
        (symbol-macrolet ((path-info (getf env :path-info)))
          (if (scan "^/api/" path-info)
              (progn
                (setf path-info (subseq path-info 4))
                (call *api* env))
              (call *web* env)))))
