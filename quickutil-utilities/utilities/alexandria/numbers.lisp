(in-package #:quickutil)

(defutil clamp (:version (1 . 0)
                :category (alexandria math))
  "Clamps the `number` into [`min`, `max`] range. Returns `min` if `number` is lesser then
`min` and `max` if `number` is greater then `max`, otherwise returns `number`."
  #>%%%>
  (declaim (inline clamp))
  (defun clamp (number min max)
    %%DOC
    (if (< number min)
        min
        (if (> number max)
            max
            number)))
  %%%)

(defutil gaussian-random (:version (1 . 0)
                          :category (alexandria math random))
  "Returns two gaussian random double floats as the primary and secondary value,
optionally constrained by `min` and `max`. Gaussian random numbers form a standard
normal distribution around `0.0d0`.

Sufficiently positive `min` or negative `max` will cause the algorithm used to
take a very long time. If `min` is positive it should be close to zero, and
similarly if `max` is negative it should be close to zero."
  #>%%%>
  (defun gaussian-random (&optional min max)
    %%DOC
    (labels ((gauss ()
               (loop
                 for x1 = (- (random 2.0d0) 1.0d0)
                 for x2 = (- (random 2.0d0) 1.0d0)
                 for w = (+ (expt x1 2) (expt x2 2))
                 when (< w 1.0d0)
                   do (let ((v (sqrt (/ (* -2.0d0 (log w)) w))))
                        (return (values (* x1 v) (* x2 v))))))
             (guard (x min max)
               (unless (<= min x max)
                 (tagbody
                  :retry
                    (multiple-value-bind (x1 x2) (gauss)
                      (when (<= min x1 max)
                        (setf x x1)
                        (go :done))
                      (when (<= min x2 max)
                        (setf x x2)
                        (go :done))
                      (go :retry))
                  :done))
               x))
      (multiple-value-bind (g1 g2) (gauss)
        (values (guard g1 (or min g1) (or max g1))
                (guard g2 (or min g2) (or max g2))))))
  %%%)


(defutil iota (:version (1 . 0)
               :category (alexandria lists))
  "Return a list of `n` numbers, starting from `start` (with numeric contagion
from `step` applied), each consequtive number being the sum of the previous one
and `step`. `start` defaults to `0` and `step` to `1`.

Examples:

    (iota 4)                      => (0 1 2 3)
    (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
    (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)"
  #>%%%>
  (declaim (inline iota))
  (defun iota (n &key (start 0) (step 1))
    %%DOC
    (declare (type (integer 0) n) (number start step))
    (loop repeat n
          ;; KLUDGE: get numeric contagion right for the first element too
          for i = (+ (- (+ start step) step)) then (+ i step)
          collect i))
  %%%)

(defutil map-iota (:version (1 . 0)
                   :category alexandria)
  "Calls `function` with `n` numbers, starting from `start` (with numeric contagion
from `step` applied), each consequtive number being the sum of the previous one
and `step`. `start` defaults to `0` and `step` to `1`. Returns `n`.

Examples:

    (map-iota #'print 3 :start 1 :step 1.0) => 3
      ;;; 1.0
      ;;; 2.0
      ;;; 3.0
"
  #>%%%>
  (declaim (inline map-iota))
  (defun map-iota (function n &key (start 0) (step 1))
    %%DOC
    (declare (type (integer 0) n) (number start step))
    (loop repeat n
          ;; KLUDGE: get numeric contagion right for the first element too
          for i = (+ start (- step step)) then (+ i step)
          do (funcall function i))
    n)
  %%%)

(defutil lerp (:version (1 . 0)
               :category (alexandria math))
  "Returns the result of linear interpolation between `a` and `b`, using the
interpolation coefficient `v`."
  #>%%%>
  (declaim (inline lerp))
  (defun lerp (v a b)
    %%DOC
    (+ a (* v (- b a))))
  %%%)

(defutil mean (:version (1 . 0)
               :category (alexandria math statistics))
  "Returns the mean of `sample`. `sample` must be a sequence of numbers."
  #>%%%>
  (declaim (inline mean))
  (defun mean (sample)
    %%DOC
    (/ (reduce #'(lambda (x y) (+ x y)) sample) (length sample)))
  %%%)

(defutil median (:version (1 . 0)
                 :depends-on copy-sequence
                 :category (alexandria math statistics))
  "Returns median of `sample`. `sample` must be a sequence of real numbers."
  #>%%%>
  (declaim (inline median))
  (defun median (sample)
    %%DOC
    (let* ((vector (sort (copy-sequence 'vector sample) #'<))
           (length (length vector))
           (middle (truncate length 2)))
      (if (oddp length)
          (aref vector middle)
          (/ (+ (aref vector middle) (aref vector (1- middle))) 2))))
  %%%)

(defutil variance (:version (1 . 0)
                   :depends-on mean
                   :category (alexandria math statistics))
  "Variance of `sample`. Returns the biased variance if `biased` is true (the default),
and the unbiased estimator of variance if `biased` is false. `sample` must be a
sequence of numbers."
  #>%%%>
  (declaim (inline variance))
  (defun variance (sample &key (biased t))
    %%DOC
    (let ((mean (mean sample)))
      (/ (reduce (lambda (a b)
                   (+ a (expt (- b mean) 2)))
                 sample
                 :initial-value 0)
         (- (length sample) (if biased 0 1)))))
  %%%)

(defutil standard-deviation (:version (1 . 0)
                             :depends-on variance
                             :category (alexandria math statistics))
  "Standard deviation of `sample`. Returns the biased standard deviation if
`biased` is true (the default), and the square root of the unbiased estimator
for variance if `biased` is false (which is not the same as the unbiased
estimator for standard deviation). `sample` must be a sequence of numbers."
  #>%%%>
  (declaim (inline standard-deviation))
  (defun standard-deviation (sample &key (biased t))
    %%DOC
    (sqrt (variance sample :biased biased)))
  %%%)

(defutil maxf (:version (1 . 0)
               :category (alexandria math orthogonality))
  "Modify-macro for `max`. Sets place designated by the first argument to the
maximum of its original value and `numbers`."
  #>%%%>
  (define-modify-macro maxf (&rest numbers) max
    %%DOC)
  %%%)

(defutil minf (:version (1 . 0)
               :category (alexandria math orthogonality))
  "Modify-macro for `min`. Sets place designated by the first argument to the
minimum of its original value and `numbers`."
  #>%%%>
  (define-modify-macro minf (&rest numbers) min
    %%DOC)
  %%%)

;;;; Factorial

;;;; Factorial from qtility benchmarked to be faster than this.

;;; KLUDGE: This is really dependant on the numbers in question: for
;;; small numbers this is larger, and vice versa. Ideally instead of a
;;; constant we would have RANGE-FAST-TO-MULTIPLY-DIRECTLY-P.
#+#:ignore
(defconstant +factorial-bisection-range-limit+ 8)

;;; KLUDGE: This is really platform dependant: ideally we would use
;;; (load-time-value (find-good-direct-multiplication-limit)) instead.
#+#:ignore
(defconstant +factorial-direct-multiplication-limit+ 13)
#+#:ignore
(defun %multiply-range (i j)
  ;; We use a a bit of cleverness here:
  ;;
  ;; 1. For large factorials we bisect in order to avoid expensive bignum
  ;;    multiplications: 1 x 2 x 3 x ... runs into bignums pretty soon,
  ;;    and once it does that all further multiplications will be with bignums.
  ;;
  ;;    By instead doing the multiplication in a tree like
  ;;       ((1 x 2) x (3 x 4)) x ((5 x 6) x (7 x 8))
  ;;    we manage to get less bignums.
  ;;
  ;; 2. Division isn't exactly free either, however, so we don't bisect
  ;;    all the way down, but multiply ranges of integers close to each
  ;;    other directly.
  ;;
  ;; For even better results it should be possible to use prime
  ;; factorization magic, but Nikodemus ran out of steam.
  ;;
  ;; KLUDGE: We support factorials of bignums, but it seems quite
  ;; unlikely anyone would ever be able to use them on a modern lisp,
  ;; since the resulting numbers are unlikely to fit in memory... but
  ;; it would be extremely unelegant to define FACTORIAL only on
  ;; fixnums, _and_ on lisps with 16 bit fixnums this can actually be
  ;; needed.
  (labels ((bisect (j k)
             (declare (type (integer 1 #.most-positive-fixnum) j k))
             (if (< (- k j) +factorial-bisection-range-limit+)
                 (multiply-range j k)
                 (let ((middle (+ j (truncate (- k j) 2))))
                   (* (bisect j middle)
                      (bisect (+ middle 1) k)))))
           (bisect-big (j k)
             (declare (type (integer 1) j k))
             (if (= j k)
                 j
                 (let ((middle (+ j (truncate (- k j) 2))))
                   (* (if (<= middle most-positive-fixnum)
                          (bisect j middle)
                          (bisect-big j middle))
                      (bisect-big (+ middle 1) k)))))
           (multiply-range (j k)
             (declare (type (integer 1 #.most-positive-fixnum) j k))
             (do ((f k (* f m))
                  (m (1- k) (1- m)))
                 ((< m j) f)
               (declare (type (integer 0 (#.most-positive-fixnum)) m)
                        (type unsigned-byte f)))))
    (bisect i j)))
#+#:ignore
(declaim (inline factorial))
#+#:ignore
(defun %factorial (n)
  (if (< n 2)
      1
      (%multiply-range 1 n)))
#+#:ignore
(defun factorial (n)
  "Factorial of non-negative integer N."
  (check-type n (integer 0))
  (%factorial n))

;;;; Combinatorics

#+#:ignore
(defun binomial-coefficient (n k)
  "Binomial coefficient of N and K, also expressed as N choose K. This is the
number of K element combinations given N choises. N must be equal to or
greater then K."
  (check-type n (integer 0))
  (check-type k (integer 0))
  (assert (>= n k))
  (if (or (zerop k) (= n k))
      1
      (let ((n-k (- n k)))
        ;; Swaps K and N-K if K < N-K because the algorithm
        ;; below is faster for bigger K and smaller N-K
        (when (< k n-k)
          (rotatef k n-k))
        (if (= 1 n-k)
            n
            ;; General case, avoid computing the 1x...xK twice:
            ;;
            ;;    N!           1x...xN          (K+1)x...xN
            ;; --------  =  ---------------- =  ------------, N>1
            ;; K!(N-K)!     1x...xK x (N-K)!       (N-K)!
            (/ (%multiply-range (+ k 1) n)
               (%factorial n-k))))))

(defutil subfactorial (:version (1 . 0)
                       :category (alexandria math))
  "Subfactorial of the non-negative integer `n`."
  #>%%%>
  (defun subfactorial (n)
    %%DOC
    (check-type n (integer 0))
    (if (zerop n)
        1
        (do ((x 1 (1+ x))
             (a 0 (* x (+ a b)))
             (b 1 a))
            ((= n x) a))))
  %%%)

(defutil count-permutations (:version (1 . 0)
                             :depends-on range-product
                             :category (alexandria math))
  "Number of `k` element permutations for a sequence of `n` objects.
`k` defaults to `n`"
  #>%%%>
  (defun count-permutations (n &optional (k n))
    %%DOC
    (check-type n (integer 0))
    (check-type k (integer 0))
    (assert (>= n k))
    (range-product (1+ (- n k)) n))
  %%%)
