(in-package :cl-user)
(defpackage quickutil-server.controller
  (:use :cl
        :ningle)
  (:import-from :dbi
                :do-sql
                :prepare
                :execute
                :fetch)
  (:import-from :multival-plist
                :getf-all)
  (:import-from :alexandria
                :when-let)
  (:import-from :quickutil-server
                :*db*)
  (:import-from :quickutil-utilities
                :*utility-registry*
                :emit-utility-code
                :pretty-print-utility-code
                :reverse-lookup)
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

(defun utility-name-to-id (name)
  (let* ((query (dbi:prepare *db* "SELECT id FROM utility WHERE name = ? LIMIT 1"))
         (result (dbi:execute query name)))
    (prog1
      (getf (dbi:fetch result) :|id|)

      ;; XXX: just for cleaning up
      (dbi:fetch result))))

(setf (route *api* "/emit")
      #'(lambda (params)
          (when *db*
            (loop for name in (getf-all params :|utility|)
                  do
               (when-let (id (utility-name-to-id (string-upcase name)))
                 (dbi:do-sql *db* "INSERT INTO utility_stats SET utility_id = ?, download_count = 1 ON DUPLICATE KEY UPDATE download_count = download_count + 1" id))))

          `(200
            (:content-type "text/plain")
            (,(handler-case
                  (with-output-to-string (s)
                    (pretty-print-utility-code
                     (emit-utility-code
                      :utilities (mapcar #'string-upcase
                                         (getf-all params :|utility|)))
                     s)
                    s)
                (type-error () nil))))))

(setf (route *api* "/reverse-lookup")
      #'(lambda (params)
          `(200
            (:content-type "text/plain")
            (,(handler-case
                  (reverse-lookup (string-upcase (getf params :|symbol|)))
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
