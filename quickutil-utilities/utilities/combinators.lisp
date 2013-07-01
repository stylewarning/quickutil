(in-package #:quickutil)

(defutil fix (:version (1 . 0)
              :category functional)
  "Apply the fixed-point combinator, also known as the Y-combinator,
to the function `F : (A -> B) -> A -> B`."
  #>%%%>
  (defun fix (f)
    %%DOC
    ((lambda (x) (funcall x x))
     (lambda (x) (funcall f (lambda (y)
                              (funcall (funcall x x) y))))))
  %%%)

(defutil flip (:version (1 . 0)
               :category functional)
  "Return a function whose argument order of a binary function `f` is reversed."
  #>%%%>
  (defun flip (f)
    %%DOC
    #'(lambda (y x)
        (funcall f x y)))
  %%%)
