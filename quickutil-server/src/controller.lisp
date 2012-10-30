(in-package :cl-user)
(defpackage quickutil-server.controller
  (:use :cl)
  (:import-from :quickutil-utilities
                :*utility-registry*
                :emit-utility-code
                :pretty-print-utility-code)
  (:import-from :quickutil-server.app
                :*web*
                :*api*)
  (:import-from :clack.response
                :headers)
  (:import-from :ningle
                :route
                :next-route
                :*request*
                :*response*)
  (:import-from :yason
                :encode))
(in-package :quickutil-server.controller)

(cl-syntax:use-syntax :annot)

;;
;; for Web interface

;; http://**/
(setf (route *web* "/")
      #'(lambda (params)
          (declare (ignore params))
          (merge-pathnames #p"templates/index.html"
                           (asdf:component-pathname
                            (asdf:find-system :quickutil-server)))))

;;
;; for API

(setf (route *api* "*")
      #'(lambda (params)
          (declare (ignore params))
          (setf (headers *response* :content-type)
                "application/json")
          (next-route)))

(setf (route *api* "/")
      #'(lambda (params)
          (declare (ignore params))
          (with-output-to-string (s)
            ;; just for testing
            (yason:encode
              (loop for key being the hash-keys in *utility-registry*
                    collect (string key))
             s)
            s)))

(setf (route *api* "/emit-utility-code.lisp")
      #'(lambda (params)
          `(200
            (:content-type "text/plain")
            (,(handler-case
                  (with-output-to-string (s)
                    (pretty-print-utility-code
                     (emit-utility-code
                      :utility (intern (string-upcase (getf params :|utility|)) :keyword))
                     s)
                    s)
                (type-error () nil))))))

;;
;; page not found

(setf (route *web* "*")
      #'(lambda (params)
          (declare (ignore params))
          (or (next-route)
              '(404 nil ("page not found")))))
