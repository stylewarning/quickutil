(in-package #:quickutil)

(defutil same-name (:version (1 . 0)
                    :category symbols)
  #1="Do symbols `a` and `b` have the same name?"
  (defun same-name (a b)
    #1#
    (string= (symbol-name a)
             (symbol-name b))))
