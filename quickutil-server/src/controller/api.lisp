(in-package :cl-user)
(defpackage quickutil-server.controller
  (:use :cl
        :ningle
        :dbi)
  (:import-from :multival-plist
                :getf-all)
  (:import-from :alexandria
                :when-let
                :ensure-list)
  (:import-from :quickutil-server
                :*db*)
  (:import-from :quickutil-server.db
                :utility-name-to-id)
  (:import-from :quickutil-utilities
                :*utility-registry*
                :emit-utility-code
                :pretty-print-utility-code
                :reverse-lookup
                :util.code)
  (:import-from :quickutil-server.app
                :*api*)
  (:import-from :quickutil-server.error
                :quickutil-server-api-error)
  (:import-from :clack.response
                :headers)
  (:import-from :yason
                :encode))
(in-package :quickutil-server.controller)

(cl-syntax:use-syntax :annot)

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

(setf (route *api* "/emit")
      #'(lambda (params)
          (let ((utilities (ensure-list (getf-all params :|utility|))))
            (when *db*
              (loop for name in utilities
                    do
                 (when-let (id (utility-name-to-id (string-upcase name)))
                   (dbi:do-sql *db* "INSERT INTO utility_stats SET utility_id = ?, download_count = 1 ON DUPLICATE KEY UPDATE download_count = download_count + 1" id))))

            `(200
              (:content-type "text/plain")
              (,(handler-case
                    (with-output-to-string (s)
                      (pretty-print-utility-code
                       (emit-utility-code
                        :utilities (mapcar #'string-upcase utilities))
                       s)
                      s)
                  (type-error () nil)))))))

(setf (route *api* "/reverse-lookup")
      #'(lambda (params)
          `(200
            (:content-type "text/plain")
            (,(handler-case
                  (reverse-lookup (string-upcase (getf params :|symbol|)))
                (type-error () nil))))))

(setf (route *api* "/source-code")
      #'(lambda (params)
          (let* ((*print-case* :downcase)
                 (name (getf params :|utility|))
                 (utility (gethash (intern (string-upcase name) :keyword)
                                   *utility-registry*)))
            (when utility
              `(200
                (:content-type "text/plain")
                (,(with-output-to-string (s)
                    (quickutil-utilities::pretty-print-utility-code (util.code utility) s))))))))

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
