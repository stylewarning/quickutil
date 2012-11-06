(in-package :cl-user)
(defpackage quickutil-server
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.static
        :clack.middleware.csrf
        :clack.middleware.session)
  (:shadow :stop)
  (:import-from :quickutil-server.app
                :*app*)
  (:import-from :cl-ppcre
                :scan
                :regex-replace))
(in-package :quickutil-server)

(cl-syntax:use-syntax :annot)

(defvar *handler* nil)

(defun build (app)
  (builder
   (<clack-middleware-static>
    :path (lambda (path)
            (when (ppcre:scan "^(?:/static/|/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
              (ppcre:regex-replace "^/static" path "")))
    :root (merge-pathnames #p"static/"
                           (asdf:component-pathname
                            (asdf:find-system :quickutil-server))))
   <clack-middleware-session>
   <clack-middleware-csrf>
   app))

@export
(defun start (&key (debug t) (port 8080))
  (setf *handler*
        (clack:clackup (build *app*) :port port :debug debug)))

@export
(defun stop ()
  (when *handler*
    (clack:stop *handler*)
    (setf *handler* nil)))
