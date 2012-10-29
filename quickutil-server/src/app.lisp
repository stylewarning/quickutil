(in-package :cl-user)
(defpackage quickutil-server.app
  (:use :cl)
  (:import-from :ningle
                :<app>))
(in-package :quickutil-server.app)

(cl-syntax:use-syntax :annot)

@export
(defvar *app* (make-instance 'ningle:<app>))
