(in-package :cl-user)
(defpackage quickutil-server.controller.web
  (:use :cl
        :ningle)
  (:import-from :quickutil-utilities
                :*utility-registry*
                :util.version
                :util.categories
                :util.code)
  (:import-from :quickutil-server.view
                :render-index
                :render-list
                :render-favorites)
  (:import-from :quickutil-server.app
                :*web*)
  (:import-from :clack.middleware.csrf
                :csrf-html-tag))
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
          :favoritep ,(not (null (member name favorites :test #'string-equal)))
          :categories ,(util.categories utility)
          :code ,(mapcar
                  #'(lambda (sexp)
                      (with-output-to-string (s)
                        (quickutil-utilities::pretty-print-utility-code sexp s)
                        s))
                  (cdr (util.code utility))))))

;;
;; for Web interface

(setf (route *web* "/")
      #'(lambda (params)
          (declare (ignore params))
          (render-index
           (list
            ;; XXX: inefficient
            :categories (remove-duplicates (loop for util in (utility-plists)
                                                 append (getf util :categories)))))))

(setf (route *web* "/list/?:category?")
      #'(lambda (params)
          (let ((*print-case* :downcase)
                (category (getf params :category)))
            (render-list
             (list
              :category category
              ;; XXX: inefficient
              :categories (remove-duplicates (loop for util in (utility-plists)
                                                   append (getf util :categories)))
              :q (getf params :|q|)
              :utilities
              (utility-plists
               #'(lambda (name utility)
                   (declare (ignore name))
                   (or (not category)
                       (member category (util.categories utility)
                               :test #'string-equal))))
              :csrf-html-tag (clack.middleware.csrf:csrf-html-tag *session*))))))

(setf (route *web* "/favorites")
      #'(lambda (params)
          (declare (ignore params))
          (let ((*print-case* :downcase)
                (favorites (gethash :favorites *session*)))
            (render-favorites
             (list
              :favorites (utility-plists
                          #'(lambda (name utility)
                              (declare (ignore utility))
                              (member name favorites :test #'string-equal)))
              ;; XXX: inefficient
              :categories (remove-duplicates (loop for util in (utility-plists)
                                                   append (getf util :categories)))
              :csrf-html-tag (clack.middleware.csrf:csrf-html-tag *session*))))))

;;
;; page not found

(setf (route *web* "*")
      #'(lambda (params)
          (declare (ignore params))
          (or (next-route)
              (asdf:system-relative-pathname
               :quickutil-server
               #P"static/html/404.html"))))
