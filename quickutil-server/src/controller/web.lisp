(in-package :cl-user)
(defpackage quickutil-server.controller.web
  (:use :cl
        :ningle)
  (:import-from :quickutil-server
                :*categories*)
  (:import-from :quickutil-utilities
                :*utility-registry*
                :util.version
                :util.provides
                :util.categories
                :util.code
                :util.dependencies
                :util.documentation)
  (:import-from :quickutil-server.view
                :render-index
                :render-list
                :render-favorites
                :render-why
                :render-how)
  (:import-from :quickutil-server.app
                :*web*)
  (:import-from :clack.middleware.csrf
                :csrf-html-tag)
  (:import-from :cl-markdown
                :markdown))
(in-package :quickutil-server.controller.web)

(cl-syntax:use-syntax :annot)

;;
;; Functions

(defun utility-plists (&optional prod)
  (loop with favorites = (gethash :favorites *session*)
        for name being the hash-keys in *utility-registry* using (hash-value utility)
        if (and utility
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
          :dependencies ,(util.dependencies utility)
          :code ,(mapcar
                  #'(lambda (sexp)
                      (with-output-to-string (s)
                        (quickutil-utilities::pretty-print-utility-code sexp s)
                        s))
                  (cdr (util.code utility))))))

;;
;; for Web interface

(setf (route *web* "*")
      #'(lambda (params)
          (declare (ignore params))
          (setf (headers *response* :content-type) "text/html")
          (or (next-route)
              (asdf:system-relative-pathname
               :quickutil-server
               #P"static/html/404.html"))))

(setf (route *web* "/")
      #'(lambda (params)
          (render-index
           (list :categories *categories*
                 :is-pjax (getf params :|_pjax|)))))

(setf (route *web* "/list/?:category?")
      #'(lambda (params)
          (let ((*print-case* :downcase)
                (category (getf params :category)))
            (render-list
             (list
              :is-pjax (getf params :|_pjax|)
              :categories *categories*
              :category category
              :q (getf params :|q|)
              :utilities
              (utility-plists
               #'(lambda (name utility)
                   (declare (ignore name))
                   (or (not category)
                       (member category (util.categories utility)
                               :test #'string-equal))))
              :csrf-html-tag (clack.middleware.csrf:csrf-html-tag *session*))))))

(setf (route *web* "/why")
      #'(lambda (params)
          (render-why
           (list :categories *categories*
                 :is-pjax (getf params :|_pjax|)))))

(setf (route *web* "/how")
      #'(lambda (params)
          (render-how
           (list :categories *categories*
                 :is-pjax (getf params :|_pjax|)))))

(setf (route *web* "/favorites")
      #'(lambda (params)
          (let ((*print-case* :downcase)
                (favorites (gethash :favorites *session*)))
            (render-favorites
             (list
              :is-pjax (getf params :|_pjax|)
              :categories *categories*
              :favorites (utility-plists
                          #'(lambda (name utility)
                              (declare (ignore utility))
                              (member name favorites :test #'string-equal)))
              :csrf-html-tag (clack.middleware.csrf:csrf-html-tag *session*))))))
