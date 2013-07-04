;;;; defutil.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:quickutil-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype util-version ()
  '(cons (integer 0) (integer 0)))

(defstruct (util (:conc-name util.))
  name
  (version '(1 . 0) :type util-version) ; (major . minor)
  runtime-dependencies
  compilation-dependencies
  categories
  (hidden nil :type boolean)
  provides
  (documentation nil :type (or null string))
  (code "" :type string))

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

(defun all-utilities ()
  "Obtain a list of all of the utilities."
  (loop :for k :being :the :hash-keys :in *utility-registry*
        :collect k))

(defun util-exists-p (name)
  "Does the utility named NAME exist in the registry?"
  (not (null (lookup-util name))))

(defun ensure-list (x)
  "Ensure that X is a list."
  (if (listp x) x (list x)))

(defun ensure-keyword (x)
  "Ensure that the symbol X is in the keyword package."
  (cond
    ((keywordp x) x)
    ((symbolp x) (intern (symbol-name x) :keyword))
    ((stringp x) (intern x :keyword))
    (t (error "Can't make ~S a keyword." x))))

(defun ensure-keyword-list (x)
  "Ensure that X is a list of keywords."
  (mapcar #'ensure-keyword (ensure-list x)))

(defvar *reverse-lookup* (make-hash-table)
  "Table indexing from provided symbols to their originating utilities.")

(defun reverse-lookup (name)
  "Find the utility associated with the provided symbol named NAME."
  (nth-value 0 (gethash (ensure-keyword name) *reverse-lookup*)))

(defun index-provides (util-name provides)
  "Index all of the provides in the list PROVIDES for the utility
named UTIL-NAME."
  (dolist (p provides)
    (let ((current (gethash p *reverse-lookup*)))
      (if (or (null current) (eql current util-name))
          (setf (gethash p *reverse-lookup*) util-name)
          (error "Trying to provide ~A from utility ~A, but it is already provided by ~A."
                 p
                 util-name
                 current)))))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop :with part-length := (length part)
            :for old-pos := 0 :then (+ pos part-length)
            :for pos := (search part string
                                :start2 old-pos
                                :test test)
            :do (write-string string out
                              :start old-pos
                              :end (or pos (length string)))
            :when pos :do (write-string replacement out)
              :while pos))) 

;;; Categories

(defvar *category-index* (make-hash-table)
  "A map from categories to utility names.")

(defun index-category (category symbol &optional (index *category-index*))
  "Add SYMBOL to the category CATEGORY."
  (pushnew symbol (gethash category index)))

(defun all-categories (&optional (index *category-index*))
  "Return a list of all of the used categories."
  (loop :for cat :being :the :hash-keys :in index
        :collect cat))

(defun utils-in-category (category &optional (index *category-index*))
  "Return a list of the utilities in the category CATEGORY."
  (nth-value 0 (gethash category index)))

;;; DEFUTIL

(defmacro defutil (name (&key version
                              compilation-depends-on
                              depends-on
                              category
                              provides
                              (hidden nil))
                   &body utility-code)
  "Define a new utility."
  (check-type name    symbol)
  (check-type version util-version)
  (check-type hidden  boolean)
  
  (let ((documentation nil)
        (provides (if provides
                      (ensure-keyword-list provides)
                      (ensure-keyword-list name)))
        (categories (ensure-keyword-list category))
        (name (ensure-keyword name)))
    
    ;; Parse documentation
    ;;
    ;; XXX FIXME: Refactor this into a subfunction.
    (cond
      ((null utility-code)       nil)
      ((null (cdr utility-code)) nil)
      ((stringp (car utility-code))
       (setf documentation (car utility-code))
       (setf utility-code  (cdr utility-code))))

    `(progn
       ;; Index the symbols provided
       (index-provides ,name ',provides)
       
       ;; Index the category of the symbol
       (mapc #'(lambda (cat)
                 (index-category cat ,name))
             ',categories)
       
       ;; Generate the registration forms.   
       (setf (gethash ',name *utility-registry*)
             (make-util :name ,name
                        :version ',version
                        :compilation-dependencies ',(ensure-keyword-list compilation-depends-on)
                        :runtime-dependencies ',(ensure-keyword-list depends-on)
                        :categories ',categories
                        :hidden ,hidden
                        :provides ',provides
                        :documentation ,documentation
                        :code (replace-all (concatenate 'string ,@utility-code)
                                           "%%DOC"
                                           ,(if documentation
                                                (prin1-to-string documentation)
                                                ""))))
       ',name)))


;;;;;;;;;;;;;;;;;;;;;;; Dependency Resolution ;;;;;;;;;;;;;;;;;;;;;;;;

(defun util-dependencies (util)
  (union (util.runtime-dependencies util)
         (util.compilation-dependencies util)))

(defun dependencies (name)
  "Get the first-order dependencies for the utility named NAME."
  (util-dependencies (lookup-util name)))

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

;;; 29 October 2012, 1:00 PM CST
;;; 
;;; The topological sorting can be improved when there are circular
;;; dependencies. As it stands, if we have
;;; 
;;;   A -> B
;;;   B -> C
;;;   C -> D
;;;   D -> C
;;; 
;;; the entire graph won't be sorted at all, when we could pare it
;;; down so we have
;;; 
;;;  (C <-> D), B, A.
;;; 
;;; By not doing this, we will get incorrect load orders.
;;; 
;;; 
;;; 29 October 2012, 3:00 PM
;;; 
;;; This issue seems to be resolved, but the ordering can still be
;;; improved.
;;; 
;;; Ordering could be improved by localizing cycles. For example,
;;; 
;;;   A -> B
;;;   B -> A
;;; 
;;;   A -> C
;;; 
;;;   C -> D
;;;   D -> C
;;; 
;;; could keep A,B together and C,D together. Perhaps the optimal is
;;; 
;;;  D C A B.

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

(defun reverse-arrows (dag)
  "Given a dag DAG, reverse the arrows."
  (loop :with table := (make-hash-table)
        :for (vertex . deps) :in dag
        :do (progn
              ;; Reverse dependencies
              (dolist (dep deps)
                (push vertex (gethash dep table)))
              
              ;; Make sure VERTEX exists in the table
              (unless (nth-value 1 (gethash vertex table))
                (setf (gethash vertex table) nil)))
        :finally (return
                   (loop :for k :being :the :hash-keys :in table
                         :collect (cons k (gethash k table))))))

(defun prune-and-sort-cycles (cyclic-dag)
  "Sort the cyclic dag CYCLIC-DAG, pruning off branches and putting
them in proper sorted order, leaving just cycles."
  (handler-case (topological-sort (reverse-arrows cyclic-dag))
    (circular-dependency-error (c)
      (let ((sorted (circular-dependency-error-sorted-order c))
            (cycles (mapcar #'car (circular-dependency-error-cycles c))))
        #+#:debug
        (format t "SORTED: ~S~%~
                   CYCLES: ~S~%"
                sorted cycles)
        (warn "Circular dependency detected. Choosing arbitrary ordering: ~S"
              cycles)
        (nreverse (append sorted cycles))))))

;;; XXX FIXME: Be more clear about mutation. It has been causing many
;;; a bug.
(defun sort-dependencies (dag)
  "Topologically sort the dependencies, with error handling."
  (handler-case (topological-sort dag)
    (circular-dependency-error (c)
      (let ((sorted (circular-dependency-error-sorted-order c))
            (sorted-cycles
              (prune-and-sort-cycles (circular-dependency-error-cycles c))))
        #+#:debug
        (format t "SORTED*: ~S~%~
                   CYCLES*: ~S~%"
                sorted sorted-cycles)
        (append sorted sorted-cycles)))))

;;; This could be a lot more efficient I'm sure.
(defun generate-util-dependency-table (&key utility
                                            (registry *utility-registry*))
  "Generate dependency table for the utility named UTILITY. If UTILITY is NIL,
  generate complete dependency table."
  (labels ((complete-table ()
             (loop :for k :being :the :hash-key :in registry
                   :for v := (gethash k registry)
                   :collect (cons k (util-dependencies v)))))
    (if (null utility)
        (complete-table)
        (loop :with deps := (all-dependencies utility)
              :for node :in (complete-table)
              :when (member (car node) deps)
                :collect node))))

(defun compute-load-order (name &optional (registry *utility-registry*))
  "Compute the load order for the utility named NAME."
  (let ((sorted (sort-dependencies
                 (copy-tree
                  (generate-util-dependency-table :utility name
                                                  :registry registry)))))
    (if (member name sorted)
        sorted
        (append sorted (list name)))))

(defun compute-combined-load-order (names &optional (registry *utility-registry*))
  "Compute the load order for the list of utility names NAMES,
optimizing redundancy out."
  (loop :for name :in names
        :append (compute-load-order name registry) :into order
        :finally (return
                   (let ((coalesced nil))
                     (dolist (sym order (nreverse coalesced))
                       (pushnew sym coalesced :test #'eql))))))

(defun compute-total-load-order (&optional (registry *utility-registry*))
  "Compute the order in which the utilities must be loaded."
  (sort-dependencies (generate-util-dependency-table :registry registry)))

;;; XXX FIXME: better error handling
(defun utilities-for (&key utilities categories symbols)
  "Find all of the utilities corresponding to the utilities UTILITIES,
the categories CATEGORIES, and the symbols SYMBOLS."
  (remove-if #'null
             (remove-duplicates
              (append
               utilities
               (mapcan #'(lambda (cat)
                           (copy-list (utils-in-category cat)))
                       categories)
               (mapcar #'reverse-lookup symbols)))))

;;; XXX: Do we want a WITH-COMPILATION-UNIT?
(defun emit-utility-code (&key utilities
                               do-not-load
                               (emit-in-package-form t)
                               (registry *utility-registry*))
  "Emit all of the source code for the utility (keyword) or
utilities (keyword list) UTILITIES in order to use it. If UTILITY is
NIL, then emit all utility source code."
  (let ((non-existent (remove-if #'util-exists-p (ensure-keyword-list utilities))))
    (cond
      (non-existent
       (format nil
               "(in-package #:quickutil)~%~
                 (~A:utility-not-found-error '~S)~%"
               (if (find-package '#:quickutil-server)
                   :quickutil-client
                   :quickutil)
               non-existent))

      ((null utilities)
       "NIL")
      
      (t
       (let* ((load-order-symbols (remove-if #'(lambda (x)
                                                 (member x do-not-load))
                                             (compute-combined-load-order
                                              (ensure-keyword-list utilities)
                                              registry)))
              (load-order (mapcar #'lookup-util load-order-symbols))
              (compilation-deps (loop :for util :in load-order
                                      :append (util.compilation-dependencies util) :into deps
                                      :finally (return (remove-duplicates deps)))))
         (flet ((compute-provided-symbols ()
                  (mapcan #'(lambda (x)
                              (copy-list (util.provides (lookup-util x))))
                          utilities)))
           (with-output-to-string (*standard-output*)
             (when emit-in-package-form
               (write-string "(in-package #:quickutil)")) (terpri)
             (write-string "(when (boundp '*utilities*)") (terpri)
             (format t     "  (setf *utilities* (union *utilities* '~S)))~%"
                     load-order-symbols)
             
             (dolist (util load-order)
               (when util
                 (let ((compile-time? (member (util.name util) compilation-deps)))
                   (when compile-time?
                     (write-string "(eval-when (:compile-toplevel :load-toplevel :execute)"))
                   
                   (write-string (util.code util))

                   (when compile-time?
                     (write-string ")                                        ; eval-when"))
                   
                   (terpri))))
             (let ((*print-case* :downcase))
               (write-string "(eval-when (:compile-toplevel :load-toplevel :execute)")
               (terpri)
               (format t "  (export '~A))~%" (compute-provided-symbols))))))))))

(defun pretty-print-utility-code (code-string &optional stream)
  "Pretty print utility code string CODE-STRING to stream STREAM."
  (flet ((clean-up (string)
           (string-right-trim '(#\Space)
                              (string-trim '(#\Newline) string))))
    (if (null stream)
        (clean-up code-string)
        (write-string (clean-up code-string) stream))))
