(in-package #:quickutil)

#+#:ignore ;; Alexandria has a version of this
(defutil copy-array (:version (1 . 0)
                     :category (arrays orthogonality))
  "Make a copy of `array`."
  (defun copy-array (array)
    (let ((dims (array-dimensions array)))
      (adjust-array
       (make-array dims
                   :element-type (array-element-type array)
                   :displaced-to array)
       dims))))

(defutil rerank-array (:version (1 . 0)
                       :depends-on copy-array
                       :category (arrays orthogonality))
  "Reshape `array` to have dimensions specified by `dimensions`. This
function makes a copy of `array`."
  #>%%%>
  (defun rerank-array (dimensions array)
    %%DOC
    (let ((copy (copy-array array)))
      (make-array dimensions :displaced-to copy)))
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
        :do (setf (svref vec i) (funcall key vi))
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
