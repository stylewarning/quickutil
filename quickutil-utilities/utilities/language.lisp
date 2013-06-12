(in-package #:quickutil)

(defutil void (:version (1 . 0)
               :category (language misc))
  #1="Do absolutely nothing, and return absolutely nothing."
  (defun void (&rest args)
    #1#
    (declare (ignore args))
    (values)))

(defutil boolean (:version (1 . 0)
                  :category (language misc))
  #1="Convert X into a Boolean value."
  (defun boolean (x)
    #1#
    (and x t)))
