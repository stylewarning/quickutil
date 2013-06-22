(in-package #:quickutil)

(defutil ensure-function (:version (1 . 0)
                          :category (alexandria functional))
  "Returns the function designated by `function-designator`:
if `function-designator` is a function, it is returned, otherwise
it must be a function name and its `fdefinition` is returned."
  #>%%%>
  ;;; To propagate return type and allow the compiler to eliminate the IF when
  ;;; it is known if the argument is function or not.
  (declaim (inline ensure-function))

  (declaim (ftype (function (t) (values function &optional))
                  ensure-function))
  (defun ensure-function (function-designator)
    %%DOC
    (if (functionp function-designator)
        function-designator
        (fdefinition function-designator)))
  %%%)

(defutil ensure-functionf (:version (1 . 0)
                           :depends-on ensure-function
                           :category (alexandria functional))
  "Multiple-place modify macro for `ensure-function`: ensures that each of
`places` contains a function."
  #>%%%>
  (define-modify-macro ensure-functionf/1 () ensure-function)

  (defmacro ensure-functionf (&rest places)
    %%DOC
    `(progn ,@(mapcar (lambda (x) `(ensure-functionf/1 ,x)) places)))
  %%%)

(defutil disjoin (:version (1 . 0)
                  :depends-on ensure-function
                  :category (alexandria functional orthogonality))
  "Returns a function that applies each of `predicate` and
`more-predicate` functions in turn to its arguments, returning the
primary value of the first predicate that returns true, without
calling the remaining predicates. If none of the predicates returns
true, `nil` is returned."
  #>%%%>
  (defun disjoin (predicate &rest more-predicates)
    %%DOC
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((predicate (ensure-function predicate))
          (more-predicates (mapcar #'ensure-function more-predicates)))
      (lambda (&rest arguments)
        (or (apply predicate arguments)
            (some (lambda (p)
                    (declare (type function p))
                    (apply p arguments))
                  more-predicates)))))
  %%%)

(defutil conjoin (:version (1 . 0)
                  :category (alexandria functional orthogonality))
  "Returns a function that applies each of `predicate` and `more-predicate`
functions in turn to its arguments, returning `nil` if any of the predicates
returns false, without calling the remaining predicates. If none of the
predicates returns false, returns the primary value of the last predicate."
  #>%%%>
  (defun conjoin (predicate &rest more-predicates)
    %%DOC
    (if (null more-predicates)
        predicate
        (lambda (&rest arguments)
          (and (apply predicate arguments)
               ;; Cannot simply use CL:EVERY because we want to return the
               ;; non-NIL value of the last predicate if all succeed.
               (do ((tail (cdr more-predicates) (cdr tail))
                    (head (car more-predicates) (car tail)))
                   ((not tail)
                    (apply head arguments))
                 (unless (apply head arguments)
                   (return nil)))))))
  %%%)

(defutil compose (:version (1 . 0)
                  :depends-on (ensure-function make-gensym-list)
                  :category (alexandria functional orthogonality))
  "Returns a function composed of `function` and `more-functions` that applies its ;
arguments to to each in turn, starting from the rightmost of `more-functions`,
and then calling the next one with the primary value of the last."
  #>%%%>
  (defun compose (function &rest more-functions)
    %%DOC
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (reduce (lambda (f g)
              (let ((f (ensure-function f))
                    (g (ensure-function g)))
                (lambda (&rest arguments)
                  (declare (dynamic-extent arguments))
                  (funcall f (apply g arguments)))))
            more-functions
            :initial-value function))

  (define-compiler-macro compose (function &rest more-functions)
    (labels ((compose-1 (funs)
               (if (cdr funs)
                   `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                   `(apply ,(car funs) arguments))))
      (let* ((args (cons function more-functions))
             (funs (make-gensym-list (length args) "COMPOSE")))
        `(let ,(loop for f in funs for arg in args
                     collect `(,f (ensure-function ,arg)))
           (declare (optimize (speed 3) (safety 1) (debug 1)))
           (lambda (&rest arguments)
             (declare (dynamic-extent arguments))
             ,(compose-1 funs))))))
  %%%)

(defutil multiple-value-compose (:version (1 . 0)
                                 :depends-on (ensure-function make-gensym-list)
                                 :category (alexandria functional orthogonality))
  "Returns a function composed of `function` and `more-functions` that applies
its arguments to each in turn, starting from the rightmost of
`more-functions`, and then calling the next one with all the return values of
the last."
  #>%%%>
  (defun multiple-value-compose (function &rest more-functions)
    %%DOC
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (reduce (lambda (f g)
              (let ((f (ensure-function f))
                    (g (ensure-function g)))
                (lambda (&rest arguments)
                  (declare (dynamic-extent arguments))
                  (multiple-value-call f (apply g arguments)))))
            more-functions
            :initial-value function))

  (define-compiler-macro multiple-value-compose (function &rest more-functions)
    (labels ((compose-1 (funs)
               (if (cdr funs)
                   `(multiple-value-call ,(car funs) ,(compose-1 (cdr funs)))
                   `(apply ,(car funs) arguments))))
      (let* ((args (cons function more-functions))
             (funs (make-gensym-list (length args) "MV-COMPOSE")))
        `(let ,(mapcar #'list funs args)
           (declare (optimize (speed 3) (safety 1) (debug 1)))
           (lambda (&rest arguments)
             (declare (dynamic-extent arguments))
             ,(compose-1 funs))))))
  %%%)

(defutil curry (:version (1 . 0)
                :compilation-depends-on (ensure-function make-gensym-list)
                :category (alexandria functional))
  "Returns a function that applies `arguments` and the arguments
it is called with to `function`."
  #>%%%>
  (defun curry (function &rest arguments)
    %%DOC
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        ;; Using M-V-C we don't need to append the arguments.
        (multiple-value-call fn (values-list arguments) (values-list more)))))

  (define-compiler-macro curry (function &rest arguments)
    (let ((curries (make-gensym-list (length arguments) "CURRY"))
          (fun (gensym "FUN")))
      `(let ((,fun (ensure-function ,function))
             ,@(mapcar #'list curries arguments))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest more)
           (apply ,fun ,@curries more)))))
  %%%)

(defutil rcurry (:version (1 . 0)
                 :depends-on ensure-function
                 :category (alexandria functional))
  "Returns a function that applies the arguments it is called
with and `arguments` to `function`."
  #>%%%>
  (defun rcurry (function &rest arguments)
    %%DOC
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        (multiple-value-call fn (values-list more) (values-list arguments)))))
  %%%)

(defutil named-lambda (:version (1 . 0)
                       :category (alexandria functional))
  "Expands into a lambda-expression within whose `body` `name` denotes the
corresponding function."
  #>%%%>
  (defmacro named-lambda (name lambda-list &body body)
    %%DOC
    `(labels ((,name ,lambda-list ,@body))
       #',name))
  %%%)
