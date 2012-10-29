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

(defun dependencies (name)
  "Get the first-order dependencies for the utility named NAME."
  (util.dependencies (lookup-util name)))

(defun all-dependencies (name)
  "Get all of the dependencies for the utility named NAME."
  (let ((deps nil)
        (deps-left (make-queue)))
    
    ;; Initial dependencies
    (dolist (dep (dependencies name))
      (enqueue deps-left dep))
    
    (loop :until (queue-empty-p deps-left)
          :do (let ((next-node (dequeue deps-left)))        
                
                (unless (member next-node deps)
                  ;; Add new dependency.
                  (push next-node deps)
                  
                  ;; Enqueue all sub-dependencies.
                  (dolist (new-dep (dependencies next-node))
                    (enqueue deps-left new-dep))))
          :finally (return deps))))

(define-condition circular-dependency-error (error)
  ((cycles :initarg :cycles
           :reader circular-dependency-error-cycles)
   (sorted-order :initarg :sorted-order
                 :reader circular-dependency-error-sorted-order))
  (:report (lambda (condition stream)
             (format stream
                     "Detected circular dependency. The cycles are ~S."
                     (circular-dependency-error-cycles condition)))))

;;; FIXME TODO: The topological sorting can be improved when there are
;;; circular dependencies. As it stands, if we have
;;; 
;;;   A -> B
;;;   B -> C
;;;   C -> D
;;;   D -> C
;;; 
;;; the entire graph won't be sorted at all, when we could pare it down so we have
;;; 
;;;  (C <-> D), B, A.
;;; 
;;; By not doing this, we will get incorrect load orders.
(defun topological-sort (dag)
  "Topologically sort the dag DAG represented as a list of

    (vertex . dependency-list)

pairs."
  (labels ((list-sinks (dag)
             (loop :for (node . deps) :in dag
                   :when (null deps)
                     :collect node :into nodes
                   :finally (return (copy-tree nodes)))))
    (let* ((sorted nil)                   ; Final sorted list.
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
                                 (error 'circular-dependency-error
                                        :cycles dag
                                        :sorted-order (nreverse sorted))))))))

(defun sort-dependencies (dag)
  "Topologically sort the dependencies, with error handling."
  (handler-case (topological-sort dag)
    (circular-dependency-error (c)
      (let ((sorted (circular-dependency-error-sorted-order c))
            (cycles (circular-dependency-error-cycles c)))
        (warn "Circular dependency detected. Choosing arbitrary ordering: ~S"
              cycles)
        (append (mapcar #'car cycles) sorted)))))

;;; This could be a lot more efficient I'm sure.
(defun generate-util-dependency-table (&key utility
                                            (registry *utility-registry*))
  "Generate dependency table for the utility named UTILITY. If UTILITY is NIL,
  generate complete dependency table."
  (labels ((complete-table ()
             (loop :for k :being :the :hash-key :in registry
                   :for v := (gethash k registry)
                   :collect (cons k (util.dependencies v)))))
    (if (null utility)
        (complete-table)
        (loop :with deps := (all-dependencies utility)
              :for node :in (complete-table)
              :when (member (car node) deps)
                :collect node))))

(defun compute-load-order (name &optional (registry *utility-registry*))
  "Compute the load order for the utility named NAME."
  (let ((sorted (sort-dependencies
                 (generate-util-dependency-table :utility name
                                                 :registry registry))))
    (if (member name sorted)
        sorted
        (append sorted (list name)))))

(defun compute-total-load-order (&optional (registry *utility-registry*))
  "Compute the order in which the utilities must be loaded."
  (sort-dependencies (generate-util-dependency-table :registry registry)))

(defun flatten-progn (code)
  "Flatten PROGN forms at depth 1. That is, flatten

    (PROGN (PROGN A) (PROGN B))

to

    (PROGN A B)."
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

(defun emit-utility-code (&key utility
                               (registry *utility-registry*))
  "Emit all of the source code for the utility UTILITY in order to use
it. If UTILITY is NIL, then emit all utility source code."
  (let ((load-order (compute-load-order utility registry)))
    (loop :for name :in load-order
          :for util := (lookup-util name)
          :when util
            :collect (util.code util) :into code
          :finally (return (flatten-progn `(progn ,@code))))))
