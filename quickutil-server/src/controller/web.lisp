(in-package :cl-user)
(defpackage quickutil-server.controller.web
  (:use :cl
        :ningle)
  (:import-from :quickutil-utilities
                :*utility-registry*
                :util.version
                :util.categories
                :util.code)
  (:import-from :quickutil-server.app
                :*web*)
  (:import-from :emb
                :execute-emb))
(in-package :quickutil-server.controller.web)

(cl-syntax:use-syntax :annot)

(defvar *template-path*
    (merge-pathnames #p"templates/"
                     (asdf:component-pathname
                      (asdf:find-system :quickutil-server))))

;;
;; Functions

(defun utility-plists (utilities)
  (loop with favorites = (gethash :favorites *session*)
        for (name . utility) in utilities
        if utility
        collect
        `(:name ,(string-downcase name)
          :version ,(format nil "~A.~A"
                     (car (util.version utility))
                     (cdr (util.version utility)))
          :favoritep ,(not (null (member name favorites :test #'string-equal)))
          :categories ,(util.categories utility)
          :code ,(cdr (util.code utility)))))

;;
;; for Web interface

(setf (route *web* "/")
      #'(lambda (params)
          (declare (ignore params))

          (execute-emb (merge-pathnames #p"index.html"
                                        *template-path*))))

(setf (route *web* "/list/?:category?")
      #'(lambda (params)
          (let ((*print-case* :downcase)
                (emb:*escape-type* :html))
            (execute-emb (merge-pathnames #p"list.html"
                                          *template-path*)
                         :env `(:current "list"
                                :category ,(getf params :category)
                                :q ,(getf params :|q|)
                                :utilities
                                ,(utility-plists
                                  (loop for name being the hash-keys in *utility-registry* using (hash-value utility)
                                                       if (or (not (getf params :category))
                                                              (member (getf params :category) (util.categories utility) :test #'string-equal))
                                                         collect (cons name utility))))))))

(setf (route *web* "/favorites")
      #'(lambda (params)
          (declare (ignore params))

          (let ((*print-case* :downcase)
                (emb:*escape-type* :html))
            (execute-emb (merge-pathnames #p"favorites.html" *template-path*)
                         :env `(:current "favorites"
                                :favorites
                                ,(utility-plists (loop for name in (gethash :favorites *session*)
                                                       ;; XXX: how to do if nothing is found
                                                       collect (cons name (gethash (intern (string-upcase name) :keyword) *utility-registry*)))))))))

;;
;; page not found

(setf (route *web* "*")
      #'(lambda (params)
          (declare (ignore params))
          (or (next-route)
              '(404 nil ("page not found")))))
