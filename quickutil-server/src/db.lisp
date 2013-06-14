(in-package :cl-user)
(defpackage quickutil-server.db
  (:use :cl
        :dbi)
  (:import-from :quickutil-server.constants
                :*db*))
(in-package :quickutil-server.db)

(cl-syntax:use-syntax :annot)

@export
(defun utility-name-to-id (name)
  (unless *db*
    (return-from utility-name-to-id))

  (let* ((query (dbi:prepare *db* "SELECT id FROM utility WHERE name = ? LIMIT 1"))
         (result (dbi:execute query (string name))))
    (prog1
      (getf (dbi:fetch result) :|id|)

      ;; XXX: just for cleaning up
      (dbi:fetch result))))
