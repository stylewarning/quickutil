(in-package #:quickutil)

;;; Author: Goheeca (github: Goheeca)
(defutil nth-digit (:version (1 . 0)
                    :category (math setters))
  "Get the `n`th digit in a rational number `number`. If `n` is
positive, it refers to digits to the left of the decimal point, and if
negative, to the right."
  #>%%%>
  (declaim (ftype (function (integer rational &optional (integer 2)) integer)
                  nth-digit))
  (defun nth-digit (n number &optional (base 10))
    %%DOC
    (declare (type integer n)
             (type rational number)
             (type (integer 2) base))
    (nth-value 0 (floor (/ (mod number (expt base (1+ n))) (expt base n)))))
  
  (define-setf-expander nth-digit (n number &optional (base 10) &environment env)
    "Set integer's digit of certain base at the given index."
    (multiple-value-bind (temps vals vars store-form access-form)
        (get-setf-expansion number env)
      (let ((store (gensym))
            (ntemp (gensym))
            (basetemp (gensym))
            (vartemp (first vars)))
        (if (cdr vars) (error "Can't expand this."))
        (values (append (list ntemp basetemp) temps)
                (append (list n base) vals)
                (list store)
                `(let ((,vartemp (+ ,access-form (* (- ,store (nth-digit ,ntemp ,access-form ,basetemp)) (expt ,basetemp ,ntemp)))))
                   (assert (<= 0 ,store  (1- ,basetemp)))
                   ,store-form
                   ,store)
                `(nth-digit ,ntemp ,access-form ,basetemp)))))
  %%%)
