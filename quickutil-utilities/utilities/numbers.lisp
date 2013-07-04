(in-package #:quickutil-utilities.utilities)

(defutil digits (:version (1 . 0)
                 :category (math integers))
  "Return a list of the digits of the non-negative integer `n` in base
`base`. By default, decimal digits are returned.

The order of the digits is such that the `k`th element of the list refers to the coefficient of `base^k`. In other words, given the resulting list

    (c0 c1 c2 ... ck)

the following identity holds:

    n = c0 + c1*base + c2*base^2 + ... + ck*base^k."
  #>%%%>
  (defun digits (n &optional (base 10))
    %%DOC
    (check-type n (integer 0))
    (check-type base (integer 2))
    (loop :with remainder
          :do (setf (values n remainder) (truncate n base))
          :collect remainder
          :until (zerop integer)))
  %%%)

;;; Author: Goheeca (github: Goheeca)
;;; Modified by: Stas Boukarev (stassats)
(defutil nth-digit (:version (1 . 0)
                    :category (math setters))
  "Get the `n`th digit in a rational number `number` in base
`base`. If `n` is positive, it refers to digits to the left of the
decimal point, and if negative, to the right."
  #>%%%>
  (declaim (ftype (function (integer rational &optional (integer 2)) integer)
                  nth-digit))
  (defun nth-digit (n number &optional (base 10))
    %%DOC
    (check-type n integer)
    (check-type number rational)
    (check-type base (integer 2))
    (let* ((lower (expt base n))
           (higher (* lower base)))
    (values (floor (mod number higher) lower))))
  
  (define-setf-expander nth-digit (n number &optional (base 10) &environment env)
    "Set the `n`th digit of a rational number `number` in base `base`."
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

;;; TODO: Allow integer/bit-vector routines to take another value to
;;; allow conversion between negative integers and two's complement
;;; bit vectors.

;;; Author: Juanito Fatas (github: JuanitoFatas)
;;; Modified: Robert Smith
(defutil bit-vector-integer (:version (1 . 0)
                             :category (math integers sequences bit-vectors))
  "Convert a bit vector `bv` to a positive integer. The bits of the
integer are ordered from most significant to least significant, unless
`least-significant-first` is true."
  #>%%%>
  (defun bit-vector-integer (bv &key least-significant-first)
    %%DOC
    (check-type bv bit-vector)
    (flet ((forward-horner (sum next)
             (declare (type unsigned-byte sum)
                      (type bit next))
             (+ (ash sum 1)
                next))
           (reverse-horner (next sum)
             (declare (type unsigned-byte sum)
                      (type bit next))
             (+ (ash sum 1)
                next)))
      (declare (dynamic-extent (function forward-horner)
                               (function reverse-horner))
               (inline forward-horner reverse-horner))
      (if least-significant-first
          (reduce #'reverse-horner bv :initial-value 0 :from-end t)
          (reduce #'forward-horner bv :initial-value 0))))
  %%%)

(defutil integer-bit-vector (:version (1 . 0)
                             :category (math integers sequences bit-vectors))
  "Convert a positive integer `n` to a bit vector. The least
significant bits will be first if `least-significant-first` is true."
#>%%%>
  (defun integer-bit-vector (n &key least-significant-first)
    %%DOC
    (declare (optimize speed))
    (check-type n unsigned-byte)
    
    (let* ((len (integer-length n))
           (bv (make-array (if (zerop len) 1 len) :element-type 'bit
                                                  :initial-element 0))
           (inc-by (if least-significant-first 1 -1)))
      (labels ((rec (n i)
                 (declare (type unsigned-byte n)
                          (type (integer -1 #.array-total-size-limit) i))
                 (if (zerop n)
                     bv
                     (multiple-value-bind (quo rem) (floor n 2)
                       (declare (type unsigned-byte quo)
                                (type bit rem))
                       (unless (zerop rem)
                         (setf (sbit bv i) 1))
                       (rec quo (+ i inc-by))))))
        (rec n (if least-significant-first 0 (1- len))))))
  %%%)
