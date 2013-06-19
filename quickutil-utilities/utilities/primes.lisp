(in-package #:quickutil)

;; Author: Takaya OCHIAI (github: tkych)
(defutil primes-below (:version (1 . 0)
                       :category math)
  "Return a sorted list of all primes below an integer `n`.

Examples:

    (primes-below 13) => (2 3 5 7 11)
    (primes-below -1) => NIL"
  ;; FIXME: when n is over (* 2 array-dimension-limit).
  ;;      : Currently (2013) this is not a problem, because for such a huge n,
  ;;      : it costs too much time to practical use.
  ;; c.f. Project Euler, Problem 10, daniel.is.fischer's overview.
  ;; Pre-optimized version:
  ;; (defun %primes-below (n)
  ;;   (let* ((sieve-bound (floor n 2))
  ;;          (sieve (make-array sieve-bound          ;odd-sieve
  ;;                             :element-type 'bit :initial-element 0))
  ;;          (cross-limit (floor (isqrt n) 2)))
  ;;     (loop :for i :from 1 :to cross-limit :do
  ;;        (when (zerop (sbit sieve i))
  ;;          (loop :for j :from (* 2 i (1+ i)) :below sieve-bound
  ;;                       :by (1+ (* 2 i)) :do
  ;;             (setf (sbit sieve j) 1))))
  ;;     (loop :for i :from 1 :below sieve-bound
  ;;           :when (zerop (sbit sieve i))
  ;;           :collect (1+ (* 2 i)) :into acc
  ;;           :finally (return (cons 2 acc)))))
  #>%%%>
  (defun %primes-below-fixnum (n)
    "%PRIMES-BELOW-FIXNUM is optimized PRIMES-BELOW for fixnum N."
    (declare (optimize (speed 3) (debug 0) (safety 0))
             (ftype (function (fixnum) list) %primes-below-fixnum)
             (type fixnum n))
    (let* ((sieve-bound (the fixnum (ash n -1)))
           (sieve (make-array sieve-bound
                              :element-type 'bit
                              :initial-element 0))
           (cross-limit (the fixnum (ash (isqrt n) -1))))
      (declare (type fixnum sieve-bound cross-limit)
               (type (simple-bit-vector *) sieve))
      (loop :for i fixnum :from 1 :to cross-limit :do
        (when (zerop (the bit (sbit sieve i)))
          (loop :for j fixnum :from (the fixnum
                                         (ash (the fixnum
                                                   (* i (the fixnum (1+ i))))
                                              1))
                  :below sieve-bound
                    :by (the fixnum
                             (1+ (the fixnum (ash i 1))))
                :do (setf (sbit sieve j) 1))))
      (loop :for i fixnum :from 1 :below sieve-bound
            :when (zerop (the bit (sbit sieve i)))
              :collect (the fixnum (1+ (the fixnum (ash i 1)))) :into acc
            :finally (return (cons 2 acc)))))

  (defun %primes-below-integer (n)
    "%PRIMES-BELOW-INTEGER is optimized PRIMES-BELOW for integer N."
    (declare (optimize (speed 3) (debug 0) (safety 0))
             (ftype (function (unsigned-byte) list) %primes-below-integer)
             (unsigned-byte n))
    (let* ((sieve-bound (the unsigned-byte (ash n -1)))
           (sieve (make-array sieve-bound
                              :element-type 'bit :initial-element 0))
           (cross-limit (the unsigned-byte (ash (isqrt n) -1))))
      (declare (type unsigned-byte sieve-bound cross-limit)
               (type (simple-bit-vector *) sieve))
      (loop :for i :of-type unsigned-byte :from 1 :to cross-limit :do
        (when (zerop (sbit sieve i))
          (loop :for j :of-type unsigned-byte
                  :from (the unsigned-byte
                             (ash (the unsigned-byte
                                       (* i (the unsigned-byte (1+ i))))
                                  1))
                    :below sieve-bound
                      :by (the unsigned-byte
                               (1+ (the unsigned-byte
                                        (ash i 1))))
                :do (setf (sbit sieve j) 1))))
      (loop :for i :of-type unsigned-byte :from 1 :below sieve-bound
            :when (zerop (the bit (sbit sieve i)))
              :collect (the unsigned-byte (1+ (the unsigned-byte (ash i 1)))) :into acc
            :finally (return (cons 2 acc)))))

  (defun primes-below (n)
    %%DOC
    (check-type n integer)
    (if (<= n 2)
        nil
        (typecase n
          (fixnum (%primes-below-fixnum n))
          (bignum (%primes-below-integer n)))))
  %%%)
