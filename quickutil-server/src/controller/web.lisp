(in-package :cl-user)
(defpackage quickutil-server.controller.web
  (:use :cl
        :ningle)
  (:import-from :lack.response
                :response-headers)
  (:import-from :quickutil-server
                :*categories*)
  (:import-from :quickutil-server.constants
                :*static-files*)
  (:import-from :quickutil-utilities
                :*utility-registry*
                :util.version
                :util.provides
                :util.categories
                :util.code
                :util.hidden
                :util-dependencies
                :util.documentation)
  (:import-from :quickutil-server.view
                :render-index
                :render-list
                :render-favorites
                :render-why
                :render-how
                :render-submit)
  (:import-from :quickutil-server.app
                :*web*)
  (:import-from :lack.middleware.csrf
                :csrf-html-tag)
  (:import-from :cl-markdown
                :markdown)
  (:import-from :assoc-utils
                :aget))
(in-package :quickutil-server.controller.web)

(cl-syntax:use-syntax :annot)

;;
;; Functions

(defun utility-plists (&optional prod)
  (loop with favorites = (gethash :favorites *session*)
        for name being the hash-keys in *utility-registry* using (hash-value utility)
        if (and utility
                (not (util.hidden utility))
                (if prod
                    (funcall prod name utility)
                    t))
          collect
        `(:name ,(string-downcase name)
          :version ,(format nil "~A.~A"
                     (car (util.version utility))
                     (cdr (util.version utility)))
          :provides ,(util.provides utility)
          :favoritep ,(not (null (member name favorites :test #'string-equal)))
          :categories ,(util.categories utility)
          :documentation ,(with-output-to-string (s)
                            (markdown (util.documentation utility)
                                      :stream s))
          :dependencies ,(util-dependencies utility))))


(defun common-template-arguments ()
  `(:categories ,*categories*
    :static-files ,*static-files*))

;;
;; for Web interface

(setf (route *web* "*")
      #'(lambda (params)
          (declare (ignore params))
          (setf (getf (response-headers *response*) :content-type) "text/html")
          (or (next-route)
              (asdf:system-relative-pathname
               :quickutil-server
               #P"static/html/404.html"))))

(setf (route *web* "/")
      #'(lambda (params)
          (render-index
           `(,@(common-template-arguments)
             :is-pjax ,(aget params "_pjax")))))

(setf (route *web* "/list/?:category?")
      #'(lambda (params)
          (let ((*print-case* :downcase)
                (category (aget params :category)))
            (render-list
             `(,@(common-template-arguments)
               :is-pjax ,(aget params "_pjax")
               :category ,category
               :q ,(aget params "q")
               :utilities
               ,(utility-plists
                 #'(lambda (name utility)
                     (declare (ignore name))
                     (or (not category)
                         (member category (util.categories utility)
                                 :test #'string-equal))))
               :csrf-html-tag ,(lack.middleware.csrf:csrf-html-tag *session*))))))

(setf (route *web* "/why")
      #'(lambda (params)
          (render-why
           `(,@(common-template-arguments)
             :is-pjax ,(aget params "_pjax")))))

(setf (route *web* "/how")
      #'(lambda (params)
          (render-how
           `(,@(common-template-arguments)
             :is-pjax ,(aget params "_pjax")))))

(setf (route *web* "/submit")
      #'(lambda (params)
          (render-submit
           `(,@(common-template-arguments)
             :is-pjax ,(aget params "_pjax")
             :csrf-html-tag ,(lack.middleware.csrf:csrf-html-tag *session*)))))

(setf (route *web* "/favorites")
      #'(lambda (params)
          (let ((*print-case* :downcase)
                (favorites (gethash :favorites *session*)))
            (render-favorites
             `(,@(common-template-arguments)
               :is-pjax ,(aget params "_pjax")
               :favorites ,(utility-plists
                           #'(lambda (name utility)
                               (declare (ignore utility))
                               (member name favorites :test #'string-equal)))
               :csrf-html-tag ,(lack.middleware.csrf:csrf-html-tag *session*))))))
