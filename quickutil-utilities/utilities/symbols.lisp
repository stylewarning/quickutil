(in-package #:quickutil)

(defutil same-name (:version (1 . 0)
                    :category symbols)
  "Do symbols `a` and `b` have the same name?"
  #>%%%>
  (defun same-name (a b)
    %%DOC
    (string= (symbol-name a)
             (symbol-name b)))
  %%%)
