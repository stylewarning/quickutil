(in-package #:quickutil)

(defutil recons (:version (1 . 0)
                 :category conses)
  #1="Reuse the cons cell OLD-CONS to create a 'new' cons cell whose car
is A and whose cdr is B."
  (defun recons (a b old-cons)
    #1#
    (psetf (car old-cons) a
           (cdr old-cons) b)
    old-cons))

(defutil copy-cons (:version (1 . 0)
                    :category (conses orthogonality))
  #1="Copy the cons cell C."
  (defun copy-cons (c)
    #1#
    (cons (car c) (cdr c))))
