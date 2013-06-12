(in-package #:quickutil)

(defutil same-name (:version (1 . 0)
                    :category symbols)
  #1="Do symbols A and B have the same name?"
  (defun same-name (a b)
    #1#
    (string= (symbol-name a)
             (symbol-name b))))
