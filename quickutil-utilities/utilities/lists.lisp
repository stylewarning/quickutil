(in-package #:quickutil)

(defutil listify (:version (1 . 0)
                  :category lists)
  (defun listify (x)
    "If X is a list, return it. Otherwise, wrap it."
    (if (listp x) x (list x))))

(defutil range (:version (1 . 0)
                :category lists)
  (defun range (start end &key (step 1) (key 'identity))
    "Return the list of numbers n such that START <= n < END and n =
START + k*STEP. If a function KEY is provided, then apply KEY to each
number."
    (assert (<= start end))
    (loop :for i :from start :below end :by step :collecting (funcall key i))))

(defutil iota (:version (1 . 0)
               :depends-on (range non-negative-p)
               :category lists)
  (defun iota (n)
    "Return [0, ..., N-1]."
    (assert (non-negative-p n))
    (range 0 n)))

(defutil iota+1 (:version (1 . 0)
                 :depends-on range
                 :category lists)
  (defun iota+1 (n)
    "Return [1, ..., N]."
    (assert (>= n 1))
    (range 1 (1+ n))))

(defutil replicate (:version (1 . 0)
                    :category lists)
  (defun replicate (n x)
    "Make a list of N copies of X."
    (declare (type (integer 0) n))
    (make-list n :initial-element x)))

;;; XXX: Make generic?
(defutil slice (:version (1 . 0)
                :category lists)
  (defun slice (list indexes)
    "Compute the slice of a list LIST at indexes INDEXES."
    (loop
      :for i :in indexes
      :collect (nth i list) :into s
      :finally (return s))))

(defutil transpose (:version (1 . 0)
                    :category lists)
  (defun transpose (lists)
    "Analog to matrix transpose for a list of lists given by LISTS."
    (loop
      :for ls := lists :then (mapcar #'cdr ls)
      :until (position-if #'null ls)
      :collecting (mapcar #'car ls))))

(defutil zip (:version (1 . 0)
              :depends-on transpose
              :category lists)
  (defun zip (&rest lists)
    "Equivalent to UNZIP. Hooray for idempotency."
    (transpose lists)))

(defutil unzip (:version (1 . 0)
                :depends-on transpose
                :category lists)
  (defun unzip (&rest lists)
    "Equivalent to ZIP. Hooray for idempotency."
    (transpose lists)))

(defutil long-zip (:version (1 . 0)
                   :category lists)
  (defun long-zip (fill &rest lists)
    "ZIP using the longest, rather than shortest list, filling with
FILL."
    (let ((longest (reduce #'max lists :key #'length)))
      (apply #'zip (loop
                     :for i :in lists
                     :collect (append i (make-list (- longest (length i))
                                                   :initial-element fill)))))))

(defutil enumerate (:version (1 . 0)
                    :category lists)
  (defun enumerate (list)
    "Equivalent to (ZIP (IOTA (LENGTH LIST)) LIST)."
    (loop
      :for i :in list
      :for j :from 0
      :collect (list j i))))
  
(defutil flatten-once (:version (1 . 0)
                       :depends-on listify
                       :category lists)
  (defun flatten-once (list)
    "Flatten LIST once."
    (reduce #'append list :key #'listify)))

(defutil flatten (:version (1 . 0)
                  :category lists)
  (defun flatten (&rest xs)
    "Flatten (and append) all lists XS completely."
    (labels ((rec (xs acc)
               (cond ((null xs)  acc)
                     ((consp xs) (rec (car xs) (rec (cdr xs) acc)))
                     (t          (cons xs acc)))))
      (rec xs nil))))

(defutil ncycle (:version (1 . 0)
                 :category lists)
  (defun ncycle (list)
    "Mutate LIST into a circlular list."
    (and list
         (setf (rest (last list)) list))))

(defutil cycle (:version (1 . 0)
                :category lists)
  (defun cycle (list)
    "Make LIST into a circular list."
    (and list
         (ncycle (copy-list list)))))

(defutil next (:version (1 . 0)
               :category lists)
  (defun nest (function initial-value count)
    "Compute a COUNT compositions of FUNCTION on INITIAL-VALUE."
    (loop
      :repeat count
      :for y := initial-value :then (funcall function y)
      :finally (return y))))

(defutil nest-list (:version (1 . 0)
                    :category lists)
  (defun nest-list (function initial-value count)
    "Compute a list of COUNT compositions of FUNCTION on INITIAL-VALUE."
    (loop
      :repeat count
      :for y := initial-value :then (funcall function y)
      :collect y)))

(defutil safe-nth (:version (1 . 0)
                   :category lists)
  (defun safe-nth (n list &optional if-out-of-bounds)
    "Find the Nth element of LIST. If N is out of bounds, return
IF-OUT-OF-BOUNDS (NIL by default)."
    (if (>= n (length list))
        if-out-of-bounds
        (nth n list))))

(defutil mapply (:version (1 . 0)
                 :category lists)
  (defun mapply (f list)
    "Apply F to each list of arguments contained within LIST and collect
the results."
    (mapcar #'(lambda (x) (apply f x)) list)))

(defutil cartesian-product (:version (1 . 0)
                            :category lists)
  (defun cartesian-product (l1 l2 &optional (f 'cl:list))
    "Compute the cartesian product of L1 and L2 as if they were
sets. Optionally, map the function F across the product."
    (loop
      :for i :in l1
      :appending (loop
                   :for j :in l2
                   :collecting (funcall f i j)))))

;;; TODO: Define a SETF method for END.
(defutil end (:version (1 . 0)
              :category lists)
  (defun end (list)
    "Return the last element of LIST and whether or not it was
  empty."
    (values (car (last list)) (null list))))

(defutil tabulate (:version (1 . 0)
                   :depends-on range
                   :category lists)
  (defun tabulate (f n)
    "Return a list evaluations of F over the integers [0,n). Mimics the
SML function of the same name."
    (range 0 n :key f)))

(defutil collect-reduce (:version (1 . 0)
                         :category lists)
  (defun collect-reduce (f list &key (initial-value (car list) initial-value-p))
    "Collects intermediate reduction results of applying F to LIST. More
  or less equivalent to (LOOP :FOR I :IN LIST :COLLECT (REDUCE F I))."
    (loop
      :for j :in (if (not initial-value-p) (cdr list) list)
      :for r := (funcall f initial-value j) :then (funcall f r j)
      :collect r)))

(defutil weave (:version (1 . 0)
                :depends-on (flatten-once transpose)
                :category lists)
  (defun weave (&rest lists)
    "Return a list whose elements alternate between LIST1 and LIST2."
    (flatten-once (transpose lists))))

(defutil riffle (:version (1 . 0)
                 :depends-on (weave replicate)
                 :category lists)
  (defun riffle (list x)
    "Insert the item X in between each element of LIST."
    (butlast (weave list (replicate (length list) x)))))

(defutil extend (:version (1 . 0)
                 :category lists)
  (defun extend (xs x)
    "Adjoin X to the end of XS."
    (append xs (list x))))

(defutil list-to-vector (:version (1 . 0)
                         :category lists)
  (defun list-to-vector (list)
    "Convert LIST into a vector."
    (make-array (length list) :initial-contents list)))

(defutil sequence-to-list (:version (1 . 0)
                           :category lists)
  (defun sequence-to-list (seq)
    "Convert the sequence SEQ into a list."
    (concatenate 'list seq)))

(defutil explode (:version (1 . 0)
                  :depends-on sequence-to-list
                  :category lists)
  (defun explode (string)
    "The classic EXPLODE function. Take a string and return a list of
  its characters."
    (sequence-to-list string)))

(defutil implode (:version (1 . 0)
                  :category lists)
  (defun implode (list-of-characters)
    "The classic IMPLODE function. Take a list of characters and return
  the corresponding string."
    (coerce (list-to-vector list-of-characters) 'string)))