(in-package #:quickutil-utilities.utilities)

(defutil same-name (:version (1 . 0)
                    :category symbols)
  "Do symbols `a` and `b` have the same name?"
  #>%%%>
  (defun same-name (a b)
    %%DOC
    (check-type a symbol)
    (check-type b symbol)
    (string= a b))
  %%%)

(defutil ensure-keyword (:version (1 . 0)
                         :category symbols)
  "Ensure that a keyword is returned for the string designator `x`."
  #>%%%>
  (defun ensure-keyword (x)
    %%DOC
    (values (intern (string x) :keyword)))
  %%%)

(defutil mkstr (:version (1 . 0)
                :category (strings on-lisp))
  "Receives any number of objects (string, symbol, keyword, char, number), extracts all printed representations, and concatenates them all into one string.

Extracted from _On Lisp_, chapter 4."
  #>%%%>
  (defun mkstr (&rest args)
    %%DOC
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))
  %%%)

(defutil symb (:version (1 . 0)
               :depends-on mkstr
               :category (symbols on-lisp))
  "Receives any number of objects, concatenates all into one string with `#'mkstr` and converts them to symbol.

Extracted from _On Lisp_, chapter 4.

See also: `symbolicate`"
  #>%%%>
  (defun symb (&rest args)
    %%DOC
    (values (intern (apply #'mkstr args))))
  %%%)
