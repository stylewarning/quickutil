(in-package #:quickutil)

(defutil copy-array (:version (1 . 0)
                     :category (alexandria arrays orthogonality))
  #1="Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (defun copy-array (array &key (element-type (array-element-type array))
                                (fill-pointer (and (array-has-fill-pointer-p array)
                                                   (fill-pointer array)))
                                (adjustable (adjustable-array-p array)))
    #1#
    (let* ((dimensions (array-dimensions array))
           (new-array (make-array dimensions
                                  :element-type element-type
                                  :adjustable adjustable
                                  :fill-pointer fill-pointer)))
      (dotimes (i (array-total-size array))
        (setf (row-major-aref new-array i)
              (row-major-aref array i)))
      new-array)))
