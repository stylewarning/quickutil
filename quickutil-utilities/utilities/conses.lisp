(in-package #:quickutil)

(defutil recons (:version (1 . 0)
                 :category conses)
  #1="Reuse the cons cell `old-cons` to create a fresh cons cell whose CAR
is `a` and whose CDR is `b`."
  (defun recons (a b old-cons)
    #1#
    (psetf (car old-cons) a
           (cdr old-cons) b)
    old-cons))

(defutil copy-cons (:version (1 . 0)
                    :category (conses orthogonality))
  #1="Copy the cons cell `c`."
  (defun copy-cons (c)
    #1#
    (cons (car c) (cdr c))))
