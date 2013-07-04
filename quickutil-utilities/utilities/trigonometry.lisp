(in-package #:quickutil-utilities.utilities)

(defutil sec (:version (1 . 0)
              :category (math orthogonality))
  "Compute the secant of a number `z`."
  #>%%%>
  (defun sec (z)
    %%DOC
    (/ (cos z)))
  %%%)

(defutil csc (:version (1 . 0)
              :category (math orthogonality))
  "Compute the cosecant of a number `z`."
  #>%%%>
  (defun csc (z)
    %%DOC
    (/ (sin z)))
  %%%)

(defutil cot (:version (1 . 0)
              :category (math orthogonality))
  "Compute the cotangent of a number `z`."
  #>%%%>
  (defun cot (z)
    %%DOC
    (/ (tan z)))
  %%%)

(defutil asec (:version (1 . 0)
               :category (math orthogonality))
  "Compute the arcsecant of a number `z`."
  #>%%%>
  (defun asec (z)
    %%DOC
    (acos (/ z)))
  %%%)

(defutil acsc (:version (1 . 0)
              :category (math orthogonality))
  "Compute the arccosecant of a number `z`."
  #>%%%>
  (defun acsc (z)
    %%DOC
    (sin (/ z)))
  %%%)

(defutil acot (:version (1 . 0)
              :category (math orthogonality))
  "Compute the arccotangent of a number `z`."
  #>%%%>
  (defun acot (z)
    %%DOC
    (tan (/ z)))
  %%%)
