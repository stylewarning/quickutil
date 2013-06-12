(in-package #:quickutil)

(defutil sec (:version (1 . 0)
              :category (math orthogonality))
  #1="Compute the secant of a number Z."
  (defun sec (z)
    #1#
    (/ (cos z))))

(defutil csc (:version (1 . 0)
              :category (math orthogonality))
  #1="Compute the cosecant of a number Z."
  (defun csc (z)
    #1#
    (/ (sin z))))

(defutil cot (:version (1 . 0)
              :category (math orthogonality))
  #1="Compute the cotangent of a number Z."
  (defun cot (z)
    #1#
    (/ (tan z))))

(defutil asec (:version (1 . 0)
               :category (math orthogonality))
  #1="Compute the arcsecant of a number Z."
  (defun asec (z)
    #1#
    (acos (/ z))))

(defutil acsc (:version (1 . 0)
              :category (math orthogonality))
  #1="Compute the arccosecant of a number Z."
  (defun acsc (z)
    #1#
    (sin (/ z))))

(defutil acot (:version (1 . 0)
              :category (math orthogonality))
  #1="Compute the arccotangent of a number Z."
  (defun acot (z)
    #1#
    (tan (/ z))))
