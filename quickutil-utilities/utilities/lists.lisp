(in-package #:quickutil)

(defutil ensure-list (:version (1 . 0)
                  :category lists)
  #1="If `x` is a list, return it. Otherwise, wrap it."
  (defun ensure-list (x)
    #1#
    (if (listp x) x (list x))))

(defutil range (:version (1 . 0)
                :category lists)
  #1="Return the list of numbers `n` such that `start <= n < end` and
`n = start + k*step` for suitable integers `k`. If a function `key` is
provided, then apply it to each number."
  (defun range (start end &key (step 1) (key 'identity))
    #1#
    (assert (<= start end))
    (loop :for i :from start :below end :by step :collecting (funcall key i))))

(defutil iota (:version (1 . 0)
               :depends-on (range non-negative-p)
               :category lists)
  #1="Return `[0, ..., n-1]`."
  (defun iota (n)
    #1#
    (assert (non-negative-p n))
    (range 0 n)))

(defutil iota+1 (:version (1 . 0)
                 :depends-on range
                 :category lists)
  #1="Return `[1, ..., n]`."
  (defun iota+1 (n)
    #1#
    (assert (>= n 1))
    (range 1 (1+ n))))

(defutil replicate (:version (1 . 0)
                    :category lists)
  #1="Make a list of `n` copies of `x`."
  (defun replicate (n x)
    #1#
    (declare (type (integer 0) n))
    (make-list n :initial-element x)))

;;; XXX: Make generic?
(defutil slice (:version (1 . 0)
                :category lists)
  #1="Compute the slice of a list `list` at indexes `indexes`."
  (defun slice (list indexes)
    #1#
    (loop
      :for i :in indexes
      :collect (nth i list) :into s
      :finally (return s))))

(defutil transpose (:version (1 . 0)
                    :category lists)
  #1="Analog to matrix transpose for a list of lists given by `lists`."
  (defun transpose (lists)
    #1#
    (loop
      :for ls := lists :then (mapcar #'cdr ls)
      :until (position-if #'null ls)
      :collecting (mapcar #'car ls))))

(defutil zip (:version (1 . 0)
              :depends-on transpose
              :category lists)
  #1="Equivalent to `unzip`."
  (defun zip (&rest lists)
    #1#
    (transpose lists)))

(defutil unzip (:version (1 . 0)
                :depends-on transpose
                :category lists)
  #1="Equivalent to `zip`."
  (defun unzip (&rest lists)
    #1#
    (transpose lists)))

(defutil long-zip (:version (1 . 0)
                   :category lists)
  #1="`zip` using the longest, rather than shortest list, filling with
`fill`."
  (defun long-zip (fill &rest lists)
    #1#
    (let ((longest (reduce #'max lists :key #'length)))
      (apply #'zip (loop
                     :for i :in lists
                     :collect (append i (make-list (- longest (length i))
                                                   :initial-element fill)))))))

(defutil enumerate (:version (1 . 0)
                    :category lists)
  #1="Equivalent to `(zip (iota (length list)) list)`."
  (defun enumerate (list)
    #1#
    (loop
      :for i :in list
      :for j :from 0
      :collect (list j i))))
  
(defutil flatten-once (:version (1 . 0)
                       :depends-on ensure-list
                       :category lists)
  #1="Flatten `list` once."
  (defun flatten-once (list)
    #1#
    (reduce #'append list :key #'ensure-list)))

(defutil flatten (:version (1 . 0)
                  :category lists)
  #1="Flatten (and append) all lists `xs` completely."
  (defun flatten (&rest xs)
    #1#
    (labels ((rec (xs acc)
               (cond ((null xs)  acc)
                     ((consp xs) (rec (car xs) (rec (cdr xs) acc)))
                     (t          (cons xs acc)))))
      (rec xs nil))))

(defutil ncycle (:version (1 . 0)
                 :category lists)
  #1="Mutate `list` into a circlular list."
  (defun ncycle (list)
    #1#
    (and list
         (setf (rest (last list)) list))))

(defutil cycle (:version (1 . 0)
                :category lists)
  #1="Make `list` into a circular list."
  (defun cycle (list)
    #1#
    (and list
         (ncycle (copy-list list)))))

(defutil nest (:version (1 . 0)
               :category lists)
  #1="Compute a `count` compositions of `function` on `initial-value`."
  (defun nest (function initial-value count)
    #1#
    (loop
      :repeat count
      :for y := initial-value :then (funcall function y)
      :finally (return y))))

(defutil nest-list (:version (1 . 0)
                    :category lists)
  #1="Compute a list of `count` compositions of `function` on `initial-value`."
  (defun nest-list (function initial-value count)
    #1#
    (loop
      :repeat count
      :for y := initial-value :then (funcall function y)
      :collect y)))

(defutil safe-nth (:version (1 . 0)
                   :category lists)
  #1="Find the `n`th element of `list`. If `n` is out of bounds, return
`if-out-of-bounds` (`nil` by default)."
  (defun safe-nth (n list &optional if-out-of-bounds)
    #1#
    (if (>= n (length list))
        if-out-of-bounds
        (nth n list))))

(defutil mapply (:version (1 . 0)
                 :category lists)
  #1="Apply `f` to each list of arguments contained within `list` and collect
the results."
  (defun mapply (f list)
    #1#
    (mapcar #'(lambda (x) (apply f x)) list)))

(defutil cartesian-product (:version (1 . 0)
                            :category lists)
  #1="Compute the cartesian product of `l1` and `l2` as if they were
sets. Optionally, map the function `f` across the product."
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
  #1="Return the last element of `list` and whether or not it was
  empty."
  (defun end (list)
    #1#
    (values (car (last list)) (null list))))

(defutil tabulate (:version (1 . 0)
                   :depends-on range
                   :category lists)
  #1="Return a list evaluations of `f` over the integers `[0,n)`. Mimics the
SML function of the same name."
  (defun tabulate (f n)
    #1#
    (range 0 n :key f)))

(defutil collect-reduce (:version (1 . 0)
                         :category lists)
  #1="Collects intermediate reduction results of applying `f` to `list`. More
  or less equivalent to `(loop :for i :in list :collect (reduce f i))`."
  (defun collect-reduce (f list &key (initial-value (car list) initial-value-p))
    #1#
    (loop
      :for j :in (if (not initial-value-p) (cdr list) list)
      :for r := (funcall f initial-value j) :then (funcall f r j)
      :collect r)))

(defutil weave (:version (1 . 0)
                :depends-on (flatten-once transpose)
                :category lists)
  #1="Return a list whose elements alternate between `list1` and `list2`."
  (defun weave (&rest lists)
    #1#
    (flatten-once (transpose lists))))

(defutil riffle (:version (1 . 0)
                 :depends-on (weave replicate)
                 :category lists)
  #1="Insert the item `x` in between each element of `list`."
  (defun riffle (list x)
    #1#
    (butlast (weave list (replicate (length list) x)))))

(defutil extend (:version (1 . 0)
                 :category lists)
  #1="Adjoin `x` to the end of `xs`."
  (defun extend (xs x)
    #1#
    (append xs (list x))))

(defutil list-to-vector (:version (1 . 0)
                         :category lists)
  #1="Convert `list` into a vector."
  (defun list-to-vector (list)
    #1#
    (make-array (length list) :initial-contents list)))

(defutil sequence-to-list (:version (1 . 0)
                           :category lists)
  #1="Convert the sequence `seq` into a list."
  (defun sequence-to-list (seq)
    #1#
    (concatenate 'list seq)))

(defutil explode (:version (1 . 0)
                  :depends-on sequence-to-list
                  :category lists)
  #1="The classic `explode` function. Take a string and return a list of
  its characters."
  (defun explode (string)
    #1#
    (sequence-to-list string)))

(defutil implode (:version (1 . 0)
                  :category lists)
  #1="The classic `implode` function. Take a list of characters and return
  the corresponding string."
  (defun implode (list-of-characters)
    #1#
    (coerce (list-to-vector list-of-characters) 'string)))
