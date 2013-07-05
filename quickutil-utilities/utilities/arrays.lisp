(in-package #:quickutil-utilities.utilities)

(defutil rerank-array (:version (1 . 0)
                       :category (arrays orthogonality))
  "Reshape `array` to have dimensions specified by `dimensions`,
possibly with a different rank than the original. The dimensions of
`array` and the given `dimensions` must have the same total number of
elements."
  #>%%%>
  (defun rerank-array (dimensions array)
    %%DOC
    (check-type array array)
    (check-type dimensions (or list unsigned-byte))
    (let ((total-size (array-total-size array)))
      ;; Check that the dimensions are compatible.
      (assert (= total-size
                 (etypecase dimensions
                   (null 0)
                   (integer dimensions)
                   (list (reduce #'* dimensions))))
              (dimensions array)
              "The given dimensions ~S and the dimensions ~S of the given ~
               array must have the same total number of elements, ~D."
              dimensions
              (array-dimensions array)
              total-size)
      
      ;; Copy the array to the new one.
      (let ((reshaped (make-array dimensions
                                  :element-type (array-element-type array))))
        (dotimes (i total-size reshaped)
          (setf (row-major-aref reshaped i)
                (row-major-aref array i))))))
  %%%)

;;; XXX: Make generic?
(defutil vector-range (:version (1 . 0)
                       :category vectors)
  "Compute the equivalent of `(coerce (range a b :step step) 'vector)`."
  #>%%%>
  (defun vector-range (a b &key (step 1)
                                (key #'identity))
    %%DOC
    (assert (< a b))
    (let* ((len (- b a))
           (vec (make-array len :element-type 'integer
                                :initial-element 0)))
      (loop
        :for i :below len
        :for vi :from a :below b :by step
        :do (setf (aref vec i) (funcall key vi))
        :finally (return vec))))
  %%%)

(defutil vector-slice (:version (1 . 0)
                       :category vectors)
  "Compute the slice of a vector `v` at indexes `indexes`."
  #>%%%>
  (defun vector-slice (v indexes)
    %%DOC
    (let ((result (make-array (length indexes))))
      (loop
        :for n :from 0
        :for i :in indexes
        :do (setf (aref result n)
                  (aref v i))
        :finally (return result))))
  %%%)

(defutil vector-associative-reduce (:version (1 . 0)
                                    :category vectors)
  "Reduce `vector` with `associative-function`, using a divide-and-conquer
method."
  #>%%%>
  (defun vector-associative-reduce (vector associative-function)
    %%DOC
    (labels ((reduce-aux (lower upper)
               (declare (fixnum lower upper))
               (case (- upper lower)
                 ((0) (svref vector lower))
                 ((1) (funcall associative-function
                               (svref vector lower)
                               (svref vector upper)))
                 (otherwise (let ((mid (floor (+ lower upper) 2)))
                              (funcall associative-function
                                       (reduce-aux lower mid)
                                       (reduce-aux (1+ mid) upper)))))))
      (reduce-aux 0 (1- (length vector)))))
  %%%)

(defutil array-contents (:version (1 . 0)
                         :category (arrays lists))
  "Extract the contents of the array `array`, returning a value that
would be suitable to pass to the `:initial-contents` keyword argument of
`make-array`."
  #>%%%>
  (defun array-contents (array)
    %%DOC
    (let ((index -1))
      (labels ((collect-dimension (dimensions)
                 (if dimensions
                     (loop :repeat (car dimensions)
                           :collect (collect-dimension (cdr dimensions)))
                     (row-major-aref array (incf index)))))
        (collect-dimension (array-dimensions array)))))
  %%%)
