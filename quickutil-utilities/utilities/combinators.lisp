(in-package #:quickutil-utilities.utilities)

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

(defutil applyable (:version (1 . 0)
                    :category functional)
  "Given a function `fun`, return a variadic function which results in `fun` being called on the passed argument list. (Note: `fun` will *not* be applied to the passed argument list.)

The resulting function is able to be applied to lists as a result (hence \"appliable\")."
  #>%%%>
  (defun applyable (fun)
    %%DOC
    (lambda (&rest args)
      (funcall fun args)))
  %%%)

(defutil applying (:version (1 . 0)
                   :category functional)
  "Given a function `fun`, return a unary function whose result is applying `fun` to the single argument."
  #>%%%>
  (defun applying (fun)
    %%DOC
    (lambda (arg)
      (apply fun arg)))
  %%%)

(defutil compose-apply (:version (1 . 0)
                        :category functional)
  "Create a variadic function whose result is applying the function `fun` to results obtained by applying each of `funs` to the argument list.

Example:

```
    (defvar average-values (compose-apply '/ '+ (applyable 'length)))
    (defvar average-list (compose-apply '/ (applying '+) 'length))

    (funcall average-values 1 2 3) => 2
    (funcall average-list '(1 2 3) => 2
```
"
  #>%%%>
  (defun compose-apply (fun &rest funs)
    %%DOC
    (lambda (&rest args)
      (apply fun (mapcar (lambda (fun) (apply fun args)) funs))))
  %%%)
