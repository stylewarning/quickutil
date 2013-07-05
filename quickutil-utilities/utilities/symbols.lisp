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
