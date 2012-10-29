;;;; defutil.lisp

(in-package #:quickutil-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype util-version ()
  '(cons (integer 0) (integer 0)))

(defstruct (util (:conc-name util.))
  (version '(1 . 0) :type util-version) ; (major . minor)
  dependencies
  code)

(defun util-major-version (util)
  "Get the major version of a utility UTIL."
  (car (util.version util)))

(defun util-minor-version (util)
  "Get the minor version of a utility UTIL."
  (cdr (util.version util)))

(defun version-greater-p (version-a version-b)
  "Is VERSION-A greater than VERSION-B?"
  (or (> (car version-a) (car version-b))
      (and (not (< (car version-a) (car version-b)))
           (> (cdr version-a) (cdr version-b)))))


;;;;;;;;;;;;;;;;;;;;;;;;; Utility Definition ;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *utility-registry* (make-hash-table)
  "Contains all utility objects which can be loaded.")

(defun lookup-util (name)
  "Lookup the utility named NAME."
  (check-type name symbol)
  (gethash name *utility-registry*))

(defmacro defutil (name (&key version depends-on) &body utility-code)
  "Define a new utility."
  (check-type name symbol)
  (check-type version util-version)
  
  `(progn
     (setf (gethash ,name *utility-registry*)
           (make-util :version ',version
                      :dependencies ',depends-on
                      :code '(progn ,@utility-code)))
     ,name))


;;;;;;;;;;;;;;;;;;;;;;; Dependency Resolution ;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-total-load-order (&optional (registry *utility-registry*))
  "Compute the order in which the utilities must be loaded."
  (labels ((list-sinks (dag)
             (loop :for (node . deps) :in dag
                   :when (null deps)
                     :collect node :into nodes
                   :finally (return (copy-tree nodes))))
           
           (generate-util-dependency-table ()
             (loop :for k :being :the :hash-key :in registry
                   :for v := (gethash k registry)
                   :collect (cons k (util.dependencies v)))))
    
    (let* ((dag    (generate-util-dependency-table))
           (sorted nil)                   ; Final sorted list.
           (sinks  (list-sinks dag)))     ; Sinks in the graph.
      (loop :while (not (null sinks))
            :do (progn
                  ;; Remove the sinks.
                  (setf dag (delete-if (lambda (x) (null (cdr x))) dag))
                  
                  ;; Get the next sink.
                  (let ((sink (pop sinks)))
                    
                    ;; Add it to the sorted list.
                    (push sink sorted)
                    
                    ;; For every node/neighborhood...
                    (dolist (node dag)
                      
                      ;; Remove the sink from the dependencies if any
                      ;; exist.
                      (setf (cdr node) (delete sink (cdr node)))
                      
                      ;; If we have no more dependencies, add it to the
                      ;; sinks.
                      (when (null (cdr node))
                        (push (car node) sinks)))))
            :finally (return (if (null dag)
                                 ;; Our DAG is empty. We're good!
                                 (nreverse sorted)
                                 
                                 ;; Our DAG isn't empty but has no
                                 ;; sinks. It must be cyclic!
                                 (error "Cannot sort a cyclic graph. ~
                                       The cycles are ~S." dag)))))))

(defun flatten-progn (code)
  (labels ((append-progn (a b)
             `(progn ,@(append (cdr a)
                               (cdr b))))
           
           (progn? (code)
             (and (listp code)
                  (eql (car code) 'progn))))
    (cond
      ((and (progn? code)
            (null (cdr code)))
       '(progn))
      
      ((not (progn? code)) `(progn ,code))
      
      (t (reduce #'append-progn (cdr code))))))

(defun emit-all-utility-code (&optional (registry *utility-registry*))
  (let ((load-order (compute-total-load-order registry)))
    (loop :for name :in load-order
          :for util := (lookup-util name)
          :when util
            :collect (util.code util) :into code
          :finally (return (flatten-progn `(progn ,@code))))))