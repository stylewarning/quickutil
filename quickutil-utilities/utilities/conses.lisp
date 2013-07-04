(in-package #:quickutil-utilities.utilities)

(defutil recons (:version (1 . 0)
                 :category conses)
  "Reuse the cons cell `old-cons`, replacing its CAR with `a` and CDR
with `b`."
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
