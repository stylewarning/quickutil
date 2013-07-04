(in-package #:quickutil-utilities.utilities)

(defutil positivep (:version (1 . 0)
                    :category (math synonyms))
  "Check if `n` is positive."
  #>%%%>
  (defun positivep (n)
    %%DOC
    (plusp n))
  %%%)

(defutil negativep (:version (1 . 0)
                    :category (math synonyms))
  "Check if `n` is negative."
  #>%%%>
  (defun negativep (n)
    %%DOC
    (minusp n))
  %%%)

(defutil non-negative-p (:version (1 . 0)
                         :category (math synonyms))
  "Check if `n` is non-negative."
  #>%%%>
  (defun non-negative-p (n)
    %%DOC
    (>= n 0))
  %%%)

(defutil non-positive-p (:version (1 . 0)
                         :category (math synonyms))
  "Check if `n` is non-positive."
  #>%%%>
  (defun non-positive-p (n)
    %%DOC
    (<= n 0))
  %%%)

(defutil non-zero-p (:version (1 . 0)
                     :category (math synonyms))
  "Check if `n` is non-zero."
  #>%%%>
  (defun non-zero-p (n)
    %%DOC
    (not (zerop n)))
  %%%)

(defutil digit-count (:version (2 . 0)
                      :category (math integers))
  "Compute the number of digits in the non-negative integer `n` in
base `base`. By default, the base is 10."
  #>%%%>
  (defun digit-count (n &optional (base 10))
    %%DOC
    (check-type n unsigned-byte)
    (check-type base (integer 2))
    (cond
      ((zerop n) 1)
      ((= base 2) (integer-length n))
      (t (let* ((approx (ceiling (integer-length n) (log base 2)))
                (exponent  (expt base (1- approx))))
           (if (> exponent n)
               (1- approx)
               approx)))))
  %%%)

(defutil range-product (:version (1 . 0)
                        :category math)
  "Compute `lower * (lower+1) * ... * (upper-1) * upper`."
  #>%%%>
  (declaim (ftype (function (integer integer) integer) range-product))
  (defun range-product (lower upper)
    %%DOC
    (assert (<= lower upper))
    (case (- upper lower)
      ((0) lower)
      ((1) (* lower upper))
      (otherwise (let ((mid (floor (+ lower upper) 2)))
                   (* (range-product lower mid)
                      (range-product (1+ mid) upper))))))
  %%%)

(defutil factorial (:version (1 . 0)
                    :depends-on range-product
                    ;; This actually supersedes Alexandria.
                    :category (alexandria math))
  "Compute the factorial of `n`, where `n! = 1 * 2 * ... * n`."
  #>%%%>
  (declaim (ftype (function ((integer 0)) (integer 1))
                  factorial))
  (defun factorial (n)
    %%DOC
    (if (zerop n)
        1
        (range-product 1 n)))
  %%%)

(defutil binomial-coefficient (:version (1 . 0)
                               :depends-on (range-product factorial)
                               ;; This actually supersedes Alexandria.
                               :category (alexandria math))
  "Binomial coefficient of `n` and `k`."
  #>%%%>
  (declaim (ftype (function ((integer 0) (integer 0)) (integer 0))
                  binomial-coefficient))
  (defun binomial-coefficient (n k)
    %%DOC
    (assert (>= n k))
    (labels ((core (k n-k)
               (if (= 1 n-k)
                   n
                   (nth-value 0 (floor (range-product (+ k 1) n)
                                       (factorial n-k))))))
      (declare (inline core)
               (ftype (function ((integer 0) (integer 0)) (integer 0))
                      core))
      (if (or (zerop k) (= n k))
          1
          (let ((n-k (- n k)))
            (declare (type (integer 0) n-k))
            (if (< k n-k)
                (core n-k k)
                (core k n-k))))))
  %%%)

;;; XXX FIXME
#+#:ignore
(defutil multinomial-coefficient (:version (1 . 0)
                                  :depends-on (binomial-coefficient
                                               collect-reduce)
                                  :category math)
  "Yield the number of combinations of `n` objects partitioned into m
groups (for `ks = (k_1, ..., k_m)`) with `k_i` objects in a respective
group (i.e., group *m* has `k_m` objects)."
  #>%%%>
  (defun multinomial-coefficient (n &rest ks)
    %%DOC
    (let ((sums (collect-reduce #'+ (sort ks #'>) :initial-value 0)))
      (reduce #'* (loop
                    :for i :in (sort ks #'>)
                    :for j :in sums
                    :collect (binomial-coefficient j i)))))
  %%%)

;;; Modify Macros

(defutil mulf (:version (1 . 0)
               :category (math orthogonality))
  "A modifying version of multiplication, similar to `incf`."
  #>%%%>
  (define-modify-macro mulf (&optional (ratio 2)) *
    %%DOC)
  %%%)

(defutil divf (:version (1 . 0)
               :category (math orthogonality))
  "A modifying version of division, similar to `decf`."
  #>%%%>
  (define-modify-macro divf (&optional (1/ratio 2)) /
    %%DOC)
  %%%)

(defutil half (:version (1 . 0)
               :category math)
  "Compute half of `x`."
  #>%%%>
  (declaim (inline half))
  (defun half (x)
    %%DOC
    (/ x 2))
  %%%)

(defutil double (:version (1 . 0)
                 :category math)
  "Compute double `x`."
  #>%%%>
  (declaim (inline double))
  (defun double (x)
    %%DOC
    (* x 2))
  %%%)

(defutil square (:version (1 . 0)
                 :category math)
  "Compute the square of `x`."
  #>%%%>
  (declaim (inline square))
  (defun square (x)
    %%DOC
    (* x x))
  %%%)

(defutil cube (:version (1 . 0)
               :category math)
  "Compute the cube of `x`."
  #>%%%>
  (declaim (inline cube))
  (defun cube (x)
    %%DOC
    (* x x x))
  %%%)
