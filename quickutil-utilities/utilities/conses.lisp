(in-package #:quickutil)

(defutil recons (:version (1 . 0)
                 :category conses)
  "Reuse the cons cell `old-cons` to create a fresh cons cell whose CAR
is `a` and whose CDR is `b`."
  #>%%%>
  (defun recons (a b old-cons)
    %%DOC
    (psetf (car old-cons) a
           (cdr old-cons) b)
    old-cons)
  %%%)

(defutil copy-cons (:version (1 . 0)
                    :category (conses orthogonality))
  "Copy the cons cell `c`."
  #>%%%>
  (defun copy-cons (c)
    %%DOC
    (cons (car c) (cdr c)))
  %%%)
