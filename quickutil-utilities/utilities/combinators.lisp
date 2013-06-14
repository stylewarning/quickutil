(in-package #:quickutil)

(defutil fix (:version (1 . 0)
              :category functional)
  #1="Apply the fixed-point combinator, also known as the Y-combinator,
to the function `F : (A -> B) -> A -> B`."
  (defun fix (f)
    #1#
    ((lambda (x) (funcall x x))
     (lambda (x) (funcall f (lambda (y)
                              (funcall (funcall x x) y)))))))
