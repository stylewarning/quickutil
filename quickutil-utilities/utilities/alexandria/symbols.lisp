(in-package #:quickutil)

(defutil ensure-symbol (:version (1 . 0)
                        :category (alexandria symbols))
  #1="Returns a symbol with name designated by NAME, accessible in package
designated by PACKAGE. If symbol is not already accessible in PACKAGE, it is
interned there. Returns a secondary value reflecting the status of the symbol
in the package, which matches the secondary return value of INTERN.

Example:

  (ensure-symbol :cons :cl) => cl:cons, :external
"
  (declaim (inline ensure-symbol))
  (defun ensure-symbol (name &optional (package *package*))
    #1#
    (intern (string name) package)))

(defutil format-symbol (:version (1 . 0)
                        :category (alexandria symbols))
  #1="Constructs a string by applying ARGUMENTS to string designator CONTROL as
if by FORMAT within WITH-STANDARD-IO-SYNTAX, and then creates a symbol named
by that string.

If PACKAGE is NIL, returns an uninterned symbol, if package is T, returns a
symbol interned in the current package, and otherwise returns a symbol
interned in the package designated by PACKAGE."
  (defun maybe-intern (name package)
    (values
     (if package
         (intern name (if (eq t package) *package* package))
         (make-symbol name))))

  (declaim (inline format-symbol))
  (defun format-symbol (package control &rest arguments)
    #1#
    (maybe-intern (with-standard-io-syntax
                    (apply #'format nil (string control) arguments))
                  package)))

(defutil make-keyword (:version (1 . 0)
                       :category (alexandria symbols))
  #1="Interns the string designated by NAME in the KEYWORD package."
  (defun make-keyword (name)
    #1#
    (intern (string name) :keyword)))

(defutil make-gensym (:version (1 . 0)
                      :category (alexandria symbols))
  #1="If NAME is a non-negative integer, calls GENSYM using it. Otherwise NAME
must be a string designator, in which case calls GENSYM using the designated
string as the argument."
  (defun make-gensym (name)
    #1#
    (gensym (if (typep name '(integer 0))
                name
                (string name)))))

(defutil make-gensym-list (:version (1 . 0)
                           :category (alexandria symbols))
  #1="Returns a list of LENGTH gensyms, each generated as if with a call to MAKE-GENSYM,
using the second (optional, defaulting to \"G\") argument."
  (defun make-gensym-list (length &optional (x "G"))
    #1#
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g)))))

(defutil symbolicate (:version (1 . 0)
                      :category (alexandria symbols))
  #1="Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (defun symbolicate (&rest things)
    #1#
    (let* ((length (reduce #'+ things
                           :key (lambda (x) (length (string x)))))
           (name (make-array length :element-type 'character)))
      (let ((index 0))
        (dolist (thing things (values (intern name)))
          (let* ((x (string thing))
                 (len (length x)))
            (replace name x :start1 index)
            (incf index len)))))))
