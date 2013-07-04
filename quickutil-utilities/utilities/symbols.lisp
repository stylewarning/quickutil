(in-package #:quickutil-utilities.utilities)

(defutil same-name (:version (1 . 0)
                    :category symbols)
  "Do symbols `a` and `b` have the same name?"
  #>%%%>
  (defun same-name (a b)
    %%DOC
    (string= (symbol-name a)
             (symbol-name b)))
  %%%)

(defutil ensure-keyword (:version (1 . 0)
                         :category symbols)
  "Ensure that a keyword is returned for the string designator `x`."
  #>%%%>
  (defun ensure-keyword (x)
    %%DOC
    (nth-value 0 (etypecase x
                   (keyword   x)
                   (symbol    (intern (symbol-name x) :keyword))
                   (string    (intern x :keyword))
                   (character (intern (string x) :keyword)))))
  %%%)
