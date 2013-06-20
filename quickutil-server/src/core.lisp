(in-package :cl-user)
(defpackage quickutil-server
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.static
        :clack.middleware.csrf
        :clack.middleware.session
        :closure-template)
  (:shadow :stop)
  (:import-from :alexandria
                :when-let
                :hash-table-keys)
  (:import-from :trivial-shell
                :shell-command)
  (:import-from :dbi
                :connect
                :disconnect
                :prepare
                :execute
                :do-sql
                :fetch)
  (:import-from :quickutil-server.app
                :*app*)
  (:import-from :quickutil-server.constants
                :*config*
                :*db*
                :*static-files*
                :load-config)
  (:import-from :quickutil-server.db
                :utility-name-to-id)
  (:import-from :quickutil-utilities
                :*utility-registry*
                :util.categories
                :util.version)
  (:import-from :cl-ppcre
                :scan
                :regex-replace)
  (:import-from :fad
                :list-directory))
(in-package :quickutil-server)

(cl-syntax:use-syntax :annot)

(defvar *handler* nil)

@export
(defvar *template-path*
    (merge-pathnames #p"templates/"
                     (asdf:component-pathname
                      (asdf:find-system :quickutil-server))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  @export
  (defun recompile-templates (&optional (path *template-path*))
    (closure-template:compile-cl-templates (fad:list-directory path))
    t))

;; (Re-)compile the templates.
(recompile-templates)


@export
(defparameter *categories*
              (loop with categories = (make-hash-table :test 'equal)
                    for utility being the hash-values in *utility-registry*
                    do
                 (loop for cat in (util.categories utility)
                       do
                    (incf (gethash (string-downcase cat) categories 0)))
                    finally
                 (return
                   (loop for cat in (sort (copy-list (hash-table-keys categories))
                                          #'string<)
                         collect `(:name ,cat
                                   :count ,(gethash cat categories))))))

(defun build (app)
  (builder
   (<clack-middleware-static>
    :path (lambda (path)
            (when (ppcre:scan "^(?:/images/|/css/|/js/|/html/|/releases/|/robot\\.txt$|/favicon.ico$)" path)
              path))
    :root (merge-pathnames #p"static/"
                           (asdf:component-pathname
                            (asdf:find-system :quickutil-server))))
   <clack-middleware-session>
   <clack-middleware-csrf>
   #'(lambda (env)
       (let ((*db* (and (getf *config* :database-params)
                        (apply #'dbi:connect (getf *config* :database-params)))))
         (unwind-protect (funcall app env)
           (when *db* (dbi:disconnect *db*)))))))

(defun insert-to-database ()
  (loop with query = (dbi:prepare *db* "SELECT * FROM utility WHERE name = ? AND version = ? LIMIT 1")
        for name being the hash-keys in *utility-registry* using (hash-value utility)
        do
     (let* ((version
             (format nil "~A.~A"
                     (car (util.version utility))
                     (cdr (util.version utility))))
            (result (dbi:execute query (string name) version))
            (record (dbi:fetch result)))

       (unless record
         (dbi:execute
          (dbi:prepare *db*
           "INSERT INTO utility (name, version) VALUES (?, ?)") (string name) version))

       ;; XXX: just for cleaning up
       (dbi:fetch result)))

  (dbi:execute (dbi:prepare *db* "DELETE FROM utility_categories"))

  (loop for name being the hash-keys in *utility-registry* using (hash-value utility)
        for id = (utility-name-to-id name)
        do
     (when id
       (let ((query (dbi:prepare *db* "INSERT INTO utility_categories SET utility_id = ?, category_name = ?")))
         (loop for cat in (util.categories utility)
               do (dbi:execute query id (string cat)))))))

(defun md5sum (file)
  (shell-command (format nil "md5sum ~A | awk '{print $1}'"
                         file)))

@export
(defun start (&rest args &key (debug t) (port 8080) &allow-other-keys)
  (load-config)

  (setf *static-files*
        (let ((static-root (asdf:system-relative-pathname
                            :quickutil-server
                            #p"static/")))
          (flet ((filepaths (paths) (mapcar
                                     #'(lambda (file)
                                         (format nil "/~A?~A"
                                                 file (md5sum (merge-pathnames file static-root))))
                                     paths)))
            (list
             :javascripts (filepaths '("js/quickutil.js"))
             :css (filepaths '("css/main.css"))))))

  (when-let (*db* (and (getf *config* :database-params)
                       (apply #'dbi:connect (getf *config* :database-params))))
    (unwind-protect (insert-to-database)
      (dbi:disconnect *db*)))

  (setf *handler*
        (apply #'clack:clackup (build *app*) args)))

@export
(defun stop ()
  (when *handler*
    (clack:stop *handler*)
    (setf *handler* nil)))
