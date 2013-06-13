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
                :when-let)
  (:import-from :dbi
                :connect
                :disconnect
                :prepare
                :execute
                :do-sql
                :fetch)
  (:import-from :quickutil-server.app
                :*app*)
  (:import-from :quickutil-utilities
                :*utility-registry*
                :util.categories
                :util.version)
  (:import-from :cl-ppcre
                :scan
                :regex-replace)
  (:import-from :fad
                :file-exists-p
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
    (closure-template:compile-cl-templates (fad:list-directory path))))

;; (Re-)compile the templates.
(recompile-templates)


@export
(defparameter *categories*
              (loop for utility being the hash-values in *utility-registry*
                    append (util.categories utility) into categories
                    finally
                 (return
                   (sort (mapcar #'string-downcase
                                 (remove-duplicates categories :test #'string-equal))
                         #'string<))))

@export
(defparameter *config* nil)

@export
(defparameter *db* nil)

(defun slurp-file (path)
  "Read a specified file and return the content as a sequence."
  (with-open-file (stream path :direction :input)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(defun load-config ()
  (let ((config-file
         (merge-pathnames "src/config.lisp"
                          (asdf:component-pathname (asdf:find-system :quickutil-server)))))
    (when (file-exists-p config-file)
      (setf *config*
            (eval
             (read-from-string
              (slurp-file config-file)))))))

(defun build (app)
  (builder
   (<clack-middleware-static>
    :path (lambda (path)
            (when (ppcre:scan "^(?:/images/|/css/|/js/|/html/|/robot\\.txt$|/favicon.ico$)" path)
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
         (dbi:do-sql *db* "INSERT INTO utility (name, version) VALUES (?, ?)" (string name) version)))))

@export
(defun start (&rest args &key (debug t) (port 8080) &allow-other-keys)
  (load-config)

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
