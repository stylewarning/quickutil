(in-package #:quickutil)

(defutil sec (:version (1 . 0)
              :category (math orthogonality))
  #1="Compute the secant of a number `z`."
  (defun sec (z)
    #1#
    (/ (cos z))))

(defutil csc (:version (1 . 0)
              :category (math orthogonality))
  #1="Compute the cosecant of a number `z`."
  (defun csc (z)
    #1#
    (/ (sin z))))

(defutil cot (:version (1 . 0)
              :category (math orthogonality))
  #1="Compute the cotangent of a number `z`."
  (defun cot (z)
    #1#
    (/ (tan z))))

(defutil asec (:version (1 . 0)
               :category (math orthogonality))
  #1="Compute the arcsecant of a number `z`."
  (defun asec (z)
    #1#
    (acos (/ z))))

(defutil acsc (:version (1 . 0)
              :category (math orthogonality))
  #1="Compute the arccosecant of a number `z`."
  (defun acsc (z)
    #1#
    (sin (/ z))))

(defutil acot (:version (1 . 0)
              :category (math orthogonality))
  #1="Compute the arccotangent of a number `z`."
  (defun acot (z)
    #1#
    (tan (/ z))))
