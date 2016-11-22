(in-package #:quickutil-utilities.utilities)

(defutil extract-function-name (:version (1 . 0)
                                :category (alexandria control))
  "Useful for macros that want to mimic the functional interface for functions
like `#'eq` and `'eq`."
  #>%%%>
  (defun extract-function-name (spec)
    %%DOC
    (if (and (consp spec)
             (member (first spec) '(quote function)))
        (second spec)
        spec))
  %%%)

(defutil switch (:version (1 . 0)
                 :compilation-depends-on (with-gensyms extract-function-name)
                 :depends-on (with-gensyms extract-function-name)
                 :provides (switch eswitch cswitch)
                 :category (alexandria control))
  "Dispatch to different branches of code based off of the value of an expression."
  #>%%%>
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun generate-switch-body (whole object clauses test key &optional default)
      (with-gensyms (value)
        (setf test (extract-function-name test))
        (setf key (extract-function-name key))
        (when (and (consp default)
                   (member (first default) '(error cerror)))
          (setf default `(,@default "No keys match in SWITCH. Testing against ~S with ~S."
                                    ,value ',test)))
        `(let ((,value (,key ,object)))
           (cond ,@(mapcar (lambda (clause)
                             (if (member (first clause) '(t otherwise))
                                 (progn
                                   (when default
                                     (error "Multiple default clauses or illegal use of a default clause in ~S."
                                            whole))
                                   (setf default `(progn ,@(rest clause)))
                                   '(()))
                                 (destructuring-bind (key-form &body forms) clause
                                   `((,test ,value ,key-form)
                                     ,@forms))))
                           clauses)
                 (t ,default))))))

  (defmacro switch (&whole whole (object &key (test 'eql) (key 'identity))
                    &body clauses)
    "Evaluates first matching clause, returning its values, or evaluates and
returns the values of `default` if no keys match."
    (generate-switch-body whole object clauses test key))

  (defmacro eswitch (&whole whole (object &key (test 'eql) (key 'identity))
                     &body clauses)
    "Like `switch`, but signals an error if no key matches."
    (generate-switch-body whole object clauses test key '(error)))

  (defmacro cswitch (&whole whole (object &key (test 'eql) (key 'identity))
                     &body clauses)
    "Like `switch`, but signals a continuable error if no key matches."
    (generate-switch-body whole object clauses test key '(cerror "Return NIL from CSWITCH.")))
  %%%)

(defutil whichever (:version (1 . 0)
                    :depends-on with-gensyms
                    :category (alexandria control))
  "Evaluates exactly one of `possibilities`, chosen at random." ;
  #>%%%>
  (defmacro whichever (&rest possibilities &environment env)
    %%DOC
    (setf possibilities (mapcar (lambda (p) (macroexpand p env)) possibilities))
    (if (every (lambda (p) (constantp p)) possibilities)
        `(svref (load-time-value (vector ,@possibilities)) (random ,(length possibilities)))
        (labels ((expand (possibilities position random-number)
                   (if (null (cdr possibilities))
                       (car possibilities)
                       (let* ((length (length possibilities))
                              (half (truncate length 2))
                              (second-half (nthcdr half possibilities))
                              (first-half (butlast possibilities (- length half))))
                         `(if (< ,random-number ,(+ position half))
                              ,(expand first-half position random-number)
                              ,(expand second-half (+ position half) random-number))))))
          (with-gensyms (random-number)
            (let ((length (length possibilities)))
              `(let ((,random-number (random ,length)))
                 ,(expand possibilities 0 random-number)))))))
  %%%)

(defutil xor (:version (1 . 0)
              :depends-on with-gensyms
              :category (alexandria control orthogonality))
  "Evaluates its arguments one at a time, from left to right. If more than one
argument evaluates to a true value no further `datums` are evaluated, and `nil` is
returned as both primary and secondary value. If exactly one argument
evaluates to true, its value is returned as the primary value after all the
arguments have been evaluated, and `t` is returned as the secondary value. If no
arguments evaluate to true `nil` is retuned as primary, and `t` as secondary
value."
  #>%%%>
  (defmacro xor (&rest datums)
    %%DOC
    (with-gensyms (xor tmp true)
      `(let (,tmp ,true)
         (block ,xor
           ,@(mapcar (lambda (datum)
                       `(if (setf ,tmp ,datum)
                            (if ,true
                                (return-from ,xor (values nil nil))
                                (setf ,true ,tmp))))
                     datums)
           (return-from ,xor (values ,true t))))))
  %%%)

(defutil nth-value-or (:version (1 . 0)
                       :depends-on (once-only with-gensyms)
                       :category (alexandria control))
  "Evaluates `form` arguments one at a time, until the `nth-value` returned by one
of the forms is true. It then returns all the values returned by evaluating
that form. If none of the forms return a true nth value, this form returns
`nil`."
  #>%%%>
  (defmacro nth-value-or (nth-value &body forms)
    %%DOC
    (once-only (nth-value)
      (with-gensyms (values)
        `(let ((,values (multiple-value-list ,(first forms))))
           (if (nth ,nth-value ,values)
               (values-list ,values)
               ,(if (rest forms)
                    `(nth-value-or ,nth-value ,@(rest forms))
                    nil))))))
  %%%)

(defutil multiple-value-prog2 (:version (1 . 0)
                               :category (alexandria control orthogonality))
  "Evaluates `first-form`, then `second-form`, and then `forms`. Yields as its value
all the value returned by `second-form`."
  #>%%%>
  (defmacro multiple-value-prog2 (first-form second-form &body forms)
    %%DOC
    `(progn ,first-form (multiple-value-prog1 ,second-form ,@forms)))
  %%%)
