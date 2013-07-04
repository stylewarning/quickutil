(in-package #:quickutil-utilities.utilities)

(defutil void (:version (1 . 0)
               :category (language misc))
  "Do absolutely nothing, and return absolutely nothing."
  #>%%%>
  (defun void (&rest args)
    %%DOC
    (declare (ignore args))
    (values))
  %%%)

(defutil ensure-boolean (:version (1 . 0)
                  :category (language misc))
  "Convert `x` into a Boolean value."
  #>%%%>
  (defun ensure-boolean (x)
    %%DOC
    (and x t))
  %%%)
