(in-package #:quickutil)

(defutil positivep (:version (1 . 0)
                    :category math)
  (defun positivep (n)
    "Check if N is positive."
    (declare (type real n))
    (plusp n)))

(defutil negativep (:version (1 . 0)
                    :category math)
  (defun negativep (n)
    "Check if N is negative."
    (declare (type real n))
    (minusp n)))

(defutil non-negative-p (:version (1 . 0)
                         :category math)
  (defun non-negative-p (n)
    "Check if N is non-negative."
    (declare (type real n))
    (>= n 0)))

(defutil non-positive-p (:version (1 . 0)
                         :category math)
  (defun non-positive-p (n)
    "Check if N is non-positive."
    (declare (type real n))
    (<= n 0)))

(defutil non-zero-p (:version (1 . 0)
                     :category math)
  (defun non-zero-p (n)
    "Check if N is non-zero."
    (declare (type real n))
    (not (zerop n))))

(defutil integral-length (:version (1 . 0)
                          :category math)
  (declaim (ftype (function (integer &optional (integer 2)) integer)
                  integral-length))
  (defun integral-length (n &optional (base 10))
    "Compute the length of an integer (integral quantity) N in base
BASE. By default, base-10 is compute, *not* base-2 as in
INTEGER-LENGTH."
    (declare (type integer n)
             (type (integer 2) base))
    (nth-value 0 (ceiling (integer-length n) (log base 2)))))

(defutil range-product (:version (1 . 0)
                        :category math)
  (declaim (ftype (function (integer integer) integer) range-product))
  (defun range-product (lower upper)
    "Compute LOWER * (LOWER+1) * ... * (UPPER-1) * UPPER."
    (assert (<= lower upper))
    (case (- upper lower)
      ((0) lower)
      ((1) (* lower upper))
      (otherwise (let ((mid (floor (+ lower upper) 2)))
                   (* (range-product lower mid)
                      (range-product (1+ mid) upper)))))))

(defutil factorial (:version (1 . 0)
                    :depends-on (range-product)
                    :category math)
  (declaim (ftype (function ((integer 0)) (integer 1))
                  factorial))
  (defun factorial (n)
    "Compute the factorial of N, N! = 1 * 2 * ... * N."
    (if (zerop n)
        1
        (range-product 1 n))))

(defutil binomial-coefficient (:version (1 . 0)
                               :depends-on (range-product factorial)
                               :category math)
  (declaim (ftype (function ((integer 0) (integer 0)) (integer 0))
                  binomial-coefficient))
  (defun binomial-coefficient (n k)
    "Binomial coefficient of N and K."
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
                (core k n-k)))))))

(defutil multinomial-coefficient (:version (1 . 0)
                                  :depends-on (binomial-coefficient
                                               collect-reduce)
                                  :category math)
  (defun multinomial-coefficient (n &rest ks)
    "Yield the number of combinations of N objects partitioned into m
groups [for ks = (k_1, ..., k_m)] with k_i objects in a respective
group (i.e., group m has k_m objects)."
    (let ((sums (collect-reduce #'+ (sort ks #'>) :initial-value 0)))
      (reduce #'* (loop
                    :for i :in (sort ks #'>)
                    :for j :in sums
                    :collect (binomial-coefficient j i))))))

;;; Modify Macros

(defutil mulf (:version (1 . 0)
               :category (math orthogonality))
  (define-modify-macro mulf (&optional (ratio 2)) *))

(defutil divf (:version (1 . 0)
               :category (math orthogonality))
  (define-modify-macro divf (&optional (ratio 2)) /))