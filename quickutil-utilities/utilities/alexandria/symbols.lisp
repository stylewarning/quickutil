(in-package #:quickutil)

(defutil ensure-symbol (:version (1 . 0)
                        :category (alexandria symbols))
  "Returns a symbol with name designated by `name`, accessible in package
designated by `package`. If symbol is not already accessible in `package`, it is
interned there. Returns a secondary value reflecting the status of the symbol
in the package, which matches the secondary return value of `intern`.

Example:

  `(ensure-symbol :cons :cl) => cl:cons, :external`
"
  #>%%%>
  (declaim (inline ensure-symbol))
  (defun ensure-symbol (name &optional (package *package*))
    %%DOC
    (intern (string name) package))
  %%%)

(defutil format-symbol (:version (1 . 0)
                        :category (alexandria symbols))
  "Constructs a string by applying `arguments` to string designator `control` as
if by `format` within `with-standard-io-syntax`, and then creates a symbol named
by that string.

If `package` is `nil`, returns an uninterned symbol, if package is `t`, returns a
symbol interned in the current package, and otherwise returns a symbol
interned in the package designated by `package`."
  #>%%%>
  (defun maybe-intern (name package)
    (values
     (if package
         (intern name (if (eq t package) *package* package))
         (make-symbol name))))

  (declaim (inline format-symbol))
  (defun format-symbol (package control &rest arguments)
    %%DOC
    (maybe-intern (with-standard-io-syntax
                    (apply #'format nil (string control) arguments))
                  package))
  %%%)

(defutil make-keyword (:version (1 . 0)
                       :category (alexandria symbols))
  "Interns the string designated by `name` in the `keyword` package."
  #>%%%>
  (defun make-keyword (name)
    %%DOC
    (intern (string name) :keyword))
  %%%)

(defutil make-gensym (:version (1 . 0)
                      :category (alexandria symbols))
  "If `name` is a non-negative integer, calls `gensym` using it. Otherwise `name`
must be a string designator, in which case calls `gensym` using the designated
string as the argument."
  #>%%%>
  (defun make-gensym (name)
    %%DOC
    (gensym (if (typep name '(integer 0))
                name
                (string name))))
  %%%)

(defutil make-gensym-list (:version (1 . 0)
                           :category (alexandria symbols))
  "Returns a list of `length` gensyms, each generated as if with a call to `make-gensym`,
using the second (optional, defaulting to \"G\") argument."
  #>%%%>
  (defun make-gensym-list (length &optional (x "G"))
    %%DOC
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g))))
  %%%)

(defutil symbolicate (:version (1 . 0)
                      :category (alexandria symbols))
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  #>%%%>
  (defun symbolicate (&rest things)
    %%DOC
    (let* ((length (reduce #'+ things
                           :key (lambda (x) (length (string x)))))
           (name (make-array length :element-type 'character)))
      (let ((index 0))
        (dolist (thing things (values (intern name)))
          (let* ((x (string thing))
                 (len (length x)))
            (replace name x :start1 index)
            (incf index len))))))
  %%%)
