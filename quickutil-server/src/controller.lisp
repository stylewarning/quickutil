(in-package :cl-user)
(defpackage quickutil-server.controller
  (:use :cl)
  (:import-from :quickutil-server.app
                :*app*)
  (:import-from :ningle
                :route))
(in-package :quickutil-server.controller)

(cl-syntax:use-syntax :annot)

(setf (route *app* "/")
      #'(lambda (params)
          (declare (ignore params))
          (merge-pathnames #p"templates/index.html"
                           (asdf:component-pathname
                            (asdf:find-system :quickutil-server)))))
