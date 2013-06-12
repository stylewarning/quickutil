(in-package #:quickutil)

(defutil listify (:version (1 . 0)
                  :category lists)
  #1="If X is a list, return it. Otherwise, wrap it."
  (defun listify (x)
    #1#
    (if (listp x) x (list x))))

(defutil range (:version (1 . 0)
                :category lists)
  #1="Return the list of numbers n such that START <= n < END and n =
START + k*STEP. If a function KEY is provided, then apply KEY to each
number."
  (defun range (start end &key (step 1) (key 'identity))
    #1#
    (assert (<= start end))
    (loop :for i :from start :below end :by step :collecting (funcall key i))))

(defutil iota (:version (1 . 0)
               :depends-on (range non-negative-p)
               :category lists)
  #1="Return [0, ..., N-1]."
  (defun iota (n)
    #1#
    (assert (non-negative-p n))
    (range 0 n)))

(defutil iota+1 (:version (1 . 0)
                 :depends-on range
                 :category lists)
  #1="Return [1, ..., N]."
  (defun iota+1 (n)
    #1#
    (assert (>= n 1))
    (range 1 (1+ n))))

(defutil replicate (:version (1 . 0)
                    :category lists)
  #1="Make a list of N copies of X."
  (defun replicate (n x)
    #1#
    (declare (type (integer 0) n))
    (make-list n :initial-element x)))

;;; XXX: Make generic?
(defutil slice (:version (1 . 0)
                :category lists)
  #1="Compute the slice of a list LIST at indexes INDEXES."
  (defun slice (list indexes)
    #1#
    (loop
      :for i :in indexes
      :collect (nth i list) :into s
      :finally (return s))))

(defutil transpose (:version (1 . 0)
                    :category lists)
  #1="Analog to matrix transpose for a list of lists given by LISTS."
  (defun transpose (lists)
    #1#
    (loop
      :for ls := lists :then (mapcar #'cdr ls)
      :until (position-if #'null ls)
      :collecting (mapcar #'car ls))))

(defutil zip (:version (1 . 0)
              :depends-on transpose
              :category lists)
  #1="Equivalent to UNZIP. Hooray for idempotency."
  (defun zip (&rest lists)
    #1#
    (transpose lists)))

(defutil unzip (:version (1 . 0)
                :depends-on transpose
                :category lists)
  #1="Equivalent to ZIP. Hooray for idempotency."
  (defun unzip (&rest lists)
    #1#
    (transpose lists)))

(defutil long-zip (:version (1 . 0)
                   :category lists)
  #1="ZIP using the longest, rather than shortest list, filling with
FILL."
  (defun long-zip (fill &rest lists)
    #1#
    (let ((longest (reduce #'max lists :key #'length)))
      (apply #'zip (loop
                     :for i :in lists
                     :collect (append i (make-list (- longest (length i))
                                                   :initial-element fill)))))))

(defutil enumerate (:version (1 . 0)
                    :category lists)
  #1="Equivalent to (ZIP (IOTA (LENGTH LIST)) LIST)."
  (defun enumerate (list)
    #1#
    (loop
      :for i :in list
      :for j :from 0
      :collect (list j i))))
  
(defutil flatten-once (:version (1 . 0)
                       :depends-on listify
                       :category lists)
  #1="Flatten LIST once."
  (defun flatten-once (list)
    #1#
    (reduce #'append list :key #'listify)))

(defutil flatten (:version (1 . 0)
                  :category lists)
  #1="Flatten (and append) all lists XS completely."
  (defun flatten (&rest xs)
    #1#
    (labels ((rec (xs acc)
               (cond ((null xs)  acc)
                     ((consp xs) (rec (car xs) (rec (cdr xs) acc)))
                     (t          (cons xs acc)))))
      (rec xs nil))))

(defutil ncycle (:version (1 . 0)
                 :category lists)
  #1="Mutate LIST into a circlular list."
  (defun ncycle (list)
    #1#
    (and list
         (setf (rest (last list)) list))))

(defutil cycle (:version (1 . 0)
                :category lists)
  #1="Make LIST into a circular list."
  (defun cycle (list)
    #1#
    (and list
         (ncycle (copy-list list)))))

(defutil next (:version (1 . 0)
               :category lists)
  #1="Compute a COUNT compositions of FUNCTION on INITIAL-VALUE."
  (defun nest (function initial-value count)
    #1#
    (loop
      :repeat count
      :for y := initial-value :then (funcall function y)
      :finally (return y))))

(defutil nest-list (:version (1 . 0)
                    :category lists)
  #1="Compute a list of COUNT compositions of FUNCTION on INITIAL-VALUE."
  (defun nest-list (function initial-value count)
    #1#
    (loop
      :repeat count
      :for y := initial-value :then (funcall function y)
      :collect y)))

(defutil safe-nth (:version (1 . 0)
                   :category lists)
  #1="Find the Nth element of LIST. If N is out of bounds, return
IF-OUT-OF-BOUNDS (NIL by default)."
  (defun safe-nth (n list &optional if-out-of-bounds)
    #1#
    (if (>= n (length list))
        if-out-of-bounds
        (nth n list))))

(defutil mapply (:version (1 . 0)
                 :category lists)
  #1="Apply F to each list of arguments contained within LIST and collect
the results."
  (defun mapply (f list)
    #1#
    (mapcar #'(lambda (x) (apply f x)) list)))

(defutil cartesian-product (:version (1 . 0)
                            :category lists)
  #1="Compute the cartesian product of L1 and L2 as if they were
sets. Optionally, map the function F across the product."
  (defun cartesian-product (l1 l2 &optional (f 'cl:list))
    #1#
    (loop
      :for i :in l1
      :appending (loop
                   :for j :in l2
                   :collecting (funcall f i j)))))

;;; TODO: Define a SETF method for END.
(defutil end (:version (1 . 0)
              :category lists)
  #1="Return the last element of LIST and whether or not it was
  empty."
  (defun end (list)
    #1#
    (values (car (last list)) (null list))))

(defutil tabulate (:version (1 . 0)
                   :depends-on range
                   :category lists)
  #1="Return a list evaluations of F over the integers [0,n). Mimics the
SML function of the same name."
  (defun tabulate (f n)
    #1#
    (range 0 n :key f)))

(defutil collect-reduce (:version (1 . 0)
                         :category lists)
  #1="Collects intermediate reduction results of applying F to LIST. More
  or less equivalent to (LOOP :FOR I :IN LIST :COLLECT (REDUCE F I))."
  (defun collect-reduce (f list &key (initial-value (car list) initial-value-p))
    #1#
    (loop
      :for j :in (if (not initial-value-p) (cdr list) list)
      :for r := (funcall f initial-value j) :then (funcall f r j)
      :collect r)))

(defutil weave (:version (1 . 0)
                :depends-on (flatten-once transpose)
                :category lists)
  #1="Return a list whose elements alternate between LIST1 and LIST2."
  (defun weave (&rest lists)
    #1#
    (flatten-once (transpose lists))))

(defutil riffle (:version (1 . 0)
                 :depends-on (weave replicate)
                 :category lists)
  #1="Insert the item X in between each element of LIST."
  (defun riffle (list x)
    #1#
    (butlast (weave list (replicate (length list) x)))))

(defutil extend (:version (1 . 0)
                 :category lists)
  #1="Adjoin X to the end of XS."
  (defun extend (xs x)
    #1#
    (append xs (list x))))

(defutil list-to-vector (:version (1 . 0)
                         :category lists)
  #1="Convert LIST into a vector."
  (defun list-to-vector (list)
    #1#
    (make-array (length list) :initial-contents list)))

(defutil sequence-to-list (:version (1 . 0)
                           :category lists)
  #1="Convert the sequence SEQ into a list."
  (defun sequence-to-list (seq)
    #1#
    (concatenate 'list seq)))

(defutil explode (:version (1 . 0)
                  :depends-on sequence-to-list
                  :category lists)
  #1="The classic EXPLODE function. Take a string and return a list of
  its characters."
  (defun explode (string)
    #1#
    (sequence-to-list string)))

(defutil implode (:version (1 . 0)
                  :category lists)
  #1="The classic IMPLODE function. Take a list of characters and return
  the corresponding string."
  (defun implode (list-of-characters)
    #1#
    (coerce (list-to-vector list-of-characters) 'string)))
