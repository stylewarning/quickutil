(in-package #:quickutil)

(defutil void (:version (1 . 0)
               :category (language misc))
  "Do absolutely nothing, and return absolutely nothing."
  (defun void (&rest args)
    (declare (ignore args))
    (values)))
