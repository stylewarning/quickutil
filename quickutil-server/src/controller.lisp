(in-package :cl-user)
(defpackage quickutil-server.controller
  (:use :cl
        :ningle)
  (:import-from :quickutil-utilities
                :*utility-registry*
                :emit-utility-code
                :pretty-print-utility-code
                :util.version
                :util.categories
                :util.code)
  (:import-from :quickutil-server.app
                :*web*
                :*api*)
  (:import-from :quickutil-server.error
                :quickutil-server-api-error)
  (:import-from :clack.response
                :headers)
  (:import-from :emb
                :execute-emb)
  (:import-from :yason
                :encode))
(in-package :quickutil-server.controller)

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

;; http://**/
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
                         :env `(:category ,(getf params :category)
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
                         :env `(:favorites
                                ,(utility-plists (loop for name in (gethash :favorites *session*)
                                                       ;; XXX: how to do if nothing is found
                                                       collect (cons name (gethash (intern (string-upcase name) :keyword) *utility-registry*)))))))))

;;
;; for API

(setf (route *api* "*")
      #'(lambda (params)
          (declare (ignore params))
          (setf (headers *response* :content-type)
                "application/json")
          (next-route)))

(setf (route *api* "*")
      #'(lambda (params)
          (declare (ignore params))
          (handler-case (next-route)
            (quickutil-server-api-error (c)
              (yason:encode-plist
               `(:|success| 0
                 :|message| ,(princ-to-string c)))))))

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

(setf (route *api* "/favorite.json" :method :post)
      #'(lambda (params)
          (unless (getf params :|utility|)
            (error 'quickutil-server-api-error
                   :format-control "`utility' is required to favorite."))

          (pushnew (getf params :|utility|) (gethash :favorites *session*))

          (with-output-to-string (s)
            (yason:encode-plist '(:|success| 1) s))))

(setf (route *api* "/unfavorite.json" :method :post)
      #'(lambda (params)
          (unless (getf params :|utility|)
            (error 'quickutil-server-api-error
                   :format-control "`utility' is required to unfavorite."))

          (symbol-macrolet ((favorites (gethash :favorites *session*)))
            (setf favorites (remove (getf params :|utility|)
                                    favorites
                                    :test #'string=)))

          (with-output-to-string (s)
            (yason:encode-plist '(:|success| 1) s))))

;;
;; page not found

(setf (route *web* "*")
      #'(lambda (params)
          (declare (ignore params))
          (or (next-route)
              '(404 nil ("page not found")))))
