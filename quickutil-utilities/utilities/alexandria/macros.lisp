(in-package #:quickutil)

(defutil with-gensyms (:version (1 . 0)
                       :depends-on string-designator
                       :provides (with-gensyms with-unique-names)
                       :category (alexandria macro-writing))
  "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

 `(symbol string-designator)`

Bare symbols appearing in `names` are equivalent to:

 `(symbol symbol)`

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
  #>%%%>
  (defmacro with-gensyms (names &body forms)
    %%DOC
    `(let ,(mapcar (lambda (name)
                     (multiple-value-bind (symbol string)
                         (etypecase name
                           (symbol
                            (values name (symbol-name name)))
                           ((cons symbol (cons string-designator null))
                            (values (first name) (string (second name)))))
                       `(,symbol (gensym ,string))))
            names)
       ,@forms))

  (defmacro with-unique-names (names &body forms)
    %%DOC
    `(with-gensyms ,names ,@forms))
  %%%)

(defutil once-only (:version (1 . 0)
                    :compilation-depends-on make-gensym-list
                    :category (alexandria macro-writing))
  "Evaluates `forms` with symbols specified in `specs` rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of `specs` must either be a symbol naming the variable to be rebound, or of
the form:

  `(symbol initform)`

Bare symbols in `specs` are equivalent to

  `(symbol symbol)`

Example:

```
(defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
  (let ((y 0)) (cons1 (incf y))) => (1 . 1)
```
"
  #>%%%>
  (defmacro once-only (specs &body forms)
    %%DOC
    (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
          (names-and-forms (mapcar (lambda (spec)
                                     (etypecase spec
                                       (list
                                        (destructuring-bind (name form) spec
                                          (cons name form)))
                                       (symbol
                                        (cons spec spec))))
                                   specs)))
      ;; bind in user-macro
      `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
              gensyms names-and-forms)
         ;; bind in final expansion
         `(let (,,@(mapcar (lambda (g n)
                             ``(,,g ,,(cdr n)))
                           gensyms names-and-forms))
            ;; bind in user-macro
            ,(let ,(mapcar (lambda (n g) (list (car n) g))
                    names-and-forms gensyms)
               ,@forms)))))
  %%%)

(defutil parse-body (:version (1 . 0)
                     :category (alexandria macro-writing))
  "Parses `body` into `(values remaining-forms declarations doc-string)`.
Documentation strings are recognized only if `documentation` is true.
Syntax errors in body are signalled and `whole` is used in the signal
arguments when given."
  #>%%%>
  (defun parse-body (body &key documentation whole)
    %%DOC
    (let ((doc nil)
          (decls nil)
          (current nil))
      (tagbody
       :declarations
         (setf current (car body))
         (when (and documentation (stringp current) (cdr body))
           (if doc
               (error "Too many documentation strings in ~S." (or whole body))
               (setf doc (pop body)))
           (go :declarations))
         (when (and (listp current) (eql (first current) 'declare))
           (push (pop body) decls)
           (go :declarations)))
      (values body (nreverse decls) doc)))
  %%%)

(defutil parse-ordinary-lambda-list (:version (1 . 0)
                                     :depends-on (make-keyword
                                                  ensure-list
                                                  simple-program-error)
                                     :category (alexandria macro-writing))
  "Parses an ordinary lambda-list, returning as multiple values:

1. Required parameters.

2. Optional parameter specifications, normalized into form:

   `(name init suppliedp)`

3. Name of the rest parameter, or `nil`.

4. Keyword parameter specifications, normalized into form:

   `((keyword-name name) init suppliedp)`

5. Boolean indicating `&allow-other-keys` presence.

6. `&aux` parameter specifications, normalized into form

   `(name init)`.

7. Existence of `&key` in the `lambda-list`.

Signals a `program-error` if `lambda-list` is malformed."
  #>%%%>
  (defun parse-ordinary-lambda-list (lambda-list &key (normalize t)
                                                      allow-specializers
                                                      (normalize-optional normalize)
                                                      (normalize-keyword normalize)
                                                      (normalize-auxilary normalize))
    %%DOC
    (let ((state :required)
          (allow-other-keys nil)
          (auxp nil)
          (required nil)
          (optional nil)
          (rest nil)
          (keys nil)
          (keyp nil)
          (aux nil))
      (labels ((fail (elt)
                 (simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
                                       elt lambda-list))
               (check-variable (elt what &optional (allow-specializers allow-specializers))
                 (unless (and (or (symbolp elt)
                                  (and allow-specializers
                                       (consp elt) (= 2 (length elt)) (symbolp (first elt))))
                              (not (constantp elt)))
                   (simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                                         what elt lambda-list)))
               (check-spec (spec what)
                 (destructuring-bind (init suppliedp) spec
                   (declare (ignore init))
                   (check-variable suppliedp what nil))))
        (dolist (elt lambda-list)
          (case elt
            (&optional
             (if (eq state :required)
                 (setf state elt)
                 (fail elt)))
            (&rest
             (if (member state '(:required &optional))
                 (setf state elt)
                 (fail elt)))
            (&key
             (if (member state '(:required &optional :after-rest))
                 (setf state elt)
                 (fail elt))
             (setf keyp t))
            (&allow-other-keys
             (if (eq state '&key)
                 (setf allow-other-keys t
                       state elt)
                 (fail elt)))
            (&aux
             (cond ((eq state '&rest)
                    (fail elt))
                   (auxp
                    (simple-program-error "Multiple ~S in ordinary lambda-list:~%  ~S"
                                          elt lambda-list))
                   (t
                    (setf auxp t
                          state elt))
                   ))
            (otherwise
             (when (member elt '#.(set-difference lambda-list-keywords
                                                  '(&optional &rest &key &allow-other-keys &aux)))
               (simple-program-error
                "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
                elt lambda-list))
             (case state
               (:required
                (check-variable elt "required parameter")
                (push elt required))
               (&optional
                (cond ((consp elt)
                       (destructuring-bind (name &rest tail) elt
                         (check-variable name "optional parameter")
                         (cond ((cdr tail)
                                (check-spec tail "optional-supplied-p parameter"))
                               (normalize-optional
                                (setf elt (append elt '(nil)))))))
                      (t
                       (check-variable elt "optional parameter")
                       (when normalize-optional
                         (setf elt (cons elt '(nil nil))))))
                (push (ensure-list elt) optional))
               (&rest
                (check-variable elt "rest parameter")
                (setf rest elt
                      state :after-rest))
               (&key
                (cond ((consp elt)
                       (destructuring-bind (var-or-kv &rest tail) elt
                         (cond ((consp var-or-kv)
                                (destructuring-bind (keyword var) var-or-kv
                                  (unless (symbolp keyword)
                                    (simple-program-error "Invalid keyword name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
                                                          keyword lambda-list))
                                  (check-variable var "keyword parameter")))
                               (t
                                (check-variable var-or-kv "keyword parameter")
                                (when normalize-keyword
                                  (setf var-or-kv (list (make-keyword var-or-kv) var-or-kv)))))
                         (if (cdr tail)
                             (check-spec tail "keyword-supplied-p parameter")
                             (when normalize-keyword
                               (setf tail (append tail '(nil)))))
                         (setf elt (cons var-or-kv tail))))
                      (t
                       (check-variable elt "keyword parameter")
                       (setf elt (if normalize-keyword
                                     (list (list (make-keyword elt) elt) nil nil)
                                     elt))))
                (push elt keys))
               (&aux
                (if (consp elt)
                    (destructuring-bind (var &optional init) elt
                      (declare (ignore init))
                      (check-variable var "&aux parameter"))
                    (progn
                      (check-variable elt "&aux parameter")
                      (setf elt (list* elt (when normalize-auxilary
                                             '(nil))))))
                (push elt aux))
               (t
                (simple-program-error "Invalid ordinary lambda-list:~%  ~S" lambda-list)))))))
      (values (nreverse required) (nreverse optional) rest (nreverse keys)
              allow-other-keys (nreverse aux) keyp)))
  %%%)

(defutil destructuring-case (:version (1 . 0)
                             :depends-on once-only
                             :provides (destructuring-case
                                        destructuring-ccase
                                        destructuring-ecase)
                             :category (alexandria macro-writing))
  "`destructuring-case`, 'destructuring-ccase` and 'destructuring-ecase` are a combination of `case` and `destructuring-bind`.
`keyform` must evaluate to af `cons`.

Clauses are of the form:

  `((case-keys . destructuring-lambda-list) form*)`

The clause whose `case-keys` matches `car` of `key`, as if by `case`, `ccase`, or `ecase`,
is selected, and `form`s are then executed with `cdr` of `key` is destructured and
Bound By The `destructuring-lambda-list`.

Example:

```
 (defun dcase (x)
   (destructuring-case x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar, ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))
     ((t &rest rest)
      (format nil \"unknown: ~S\" rest))))

  (dcase (list :foo 1 2))        ; => \"foo: 1, 2\"
  (dcase (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (dcase (list :alt1 1))         ; => \"alt: 1\"
  (dcase (list :alt2 2))         ; => \"alt: 2\"
  (dcase (list :quux 1 2 3))     ; => \"unknown: 1, 2, 3\"

 (defun decase (x)
   (destructuring-case x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar, ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))))

  (decase (list :foo 1 2))        ; => \"foo: 1, 2\"
  (decase (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (decase (list :alt1 1))         ; => \"alt: 1\"
  (decase (list :alt2 2))         ; => \"alt: 2\"
  (decase (list :quux 1 2 3))     ; =| error
```
"
  #>%%%>
  (defun expand-destructuring-case (key clauses case)
    (once-only (key)
      `(if (typep ,key 'cons)
           (,case (car ,key)
             ,@(mapcar (lambda (clause)
                         (destructuring-bind ((keys . lambda-list) &body body) clause
                           `(,keys
                             (destructuring-bind ,lambda-list (cdr ,key)
                               ,@body))))
                clauses))
           (error "Invalid key to DESTRUCTURING-~S: ~S" ',case ,key))))

  (defmacro destructuring-case (keyform &body clauses)
    %%DOC
    (expand-destructuring-case keyform clauses 'case))
  
  (defmacro destructuring-ccase (keyform &body clauses)
    %%DOC
    (expand-destructuring-case keyform clauses 'ccase))

  (defmacro destructuring-ecase (keyform &body clauses)
    %%DOC
    (expand-destructuring-case keyform clauses 'ecase))
  %%%)
