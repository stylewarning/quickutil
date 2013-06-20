(in-package #:quickutil)

(defutil range (:version (1 . 0)
                :category lists)
  "Return the list of numbers `n` such that `start <= n < end` and
`n = start + k*step` for suitable integers `k`. If a function `key` is
provided, then apply it to each number."
  #>%%%>
  (defun range (start end &key (step 1) (key 'identity))
    %%DOC
    (assert (<= start end))
    (loop :for i :from start :below end :by step :collecting (funcall key i)))
  %%%)

(defutil iota (:version (1 . 0)
               :depends-on (range non-negative-p)
               :category lists)
  "Return `[0, ..., n-1]`."
  #>%%%>
  (defun iota (n)
    %%DOC
    (assert (non-negative-p n))
    (range 0 n))
  %%%)

(defutil iota+1 (:version (1 . 0)
                 :depends-on range
                 :category lists)
  "Return `[1, ..., n]`."
  #>%%%>
  (defun iota+1 (n)
    %%DOC
    (assert (>= n 1))
    (range 1 (1+ n)))
  %%%)

(defutil replicate (:version (1 . 0)
                    :category lists)
  "Make a list of `n` copies of `x`."
  #>%%%>
  (defun replicate (n x)
    %%DOC
    (declare (type (integer 0) n))
    (make-list n :initial-element x))
  %%%)

;;; XXX: Make generic?
(defutil slice (:version (1 . 0)
                :category lists)
  "Compute the slice of a list `list` at indexes `indexes`."
  #>%%%>
  (defun slice (list indexes)
    %%DOC
    (loop
      :for i :in indexes
      :collect (nth i list) :into s
      :finally (return s)))
  %%%)

(defutil transpose (:version (1 . 0)
                    :category lists)
  "Analog to matrix transpose for a list of lists given by `lists`."
  #>%%%>
  (defun transpose (lists)
    %%DOC
    (loop
      :for ls := lists :then (mapcar #'cdr ls)
      :until (position-if #'null ls)
      :collecting (mapcar #'car ls)))
  %%%)

(defutil zip (:version (1 . 0)
              :depends-on transpose
              :category lists)
  "Equivalent to `unzip`."
  #>%%%>
  (defun zip (&rest lists)
    %%DOC
    (transpose lists))
  %%%)

(defutil unzip (:version (1 . 0)
                :depends-on transpose
                :category lists)
  "Equivalent to `zip`."
  #>%%%>
  (defun unzip (&rest lists)
    %%DOC
    (transpose lists))
  %%%)

(defutil long-zip (:version (1 . 0)
                   :depends-on zip
                   :category lists)
  "`zip` using the longest, rather than shortest list, filling with
`fill`."
  #>%%%>
  (defun long-zip (fill &rest lists)
    %%DOC
    (let ((longest (reduce #'max lists :key #'length)))
      (apply #'zip (loop
                     :for i :in lists
                     :collect (append i (make-list (- longest (length i))
                                                   :initial-element fill))))))
  %%%)

(defutil enumerate (:version (1 . 0)
                    :category lists)
  "Equivalent to `(zip (iota (length list)) list)`."
  #>%%%>
  (defun enumerate (list)
    %%DOC
    (loop
      :for i :in list
      :for j :from 0
      :collect (list j i)))
  %%%)
  
(defutil flatten-once (:version (1 . 0)
                       :depends-on ensure-list
                       :category lists)
  "Flatten `list` once."
  #>%%%>
  (defun flatten-once (list)
    %%DOC
    (reduce #'append list :key #'ensure-list))
  %%%)

(defutil flatten (:version (1 . 0)
                  :category lists)
  "Flatten (and append) all lists `xs` completely."
  #>%%%>
  (defun flatten (&rest xs)
    %%DOC
    (labels ((rec (xs acc)
               (cond ((null xs)  acc)
                     ((consp xs) (rec (car xs) (rec (cdr xs) acc)))
                     (t          (cons xs acc)))))
      (rec xs nil)))
  %%%)

(defutil ncycle (:version (1 . 0)
                 :category lists)
  "Mutate `list` into a circlular list."
  #>%%%>
  (defun ncycle (list)
    %%DOC
    (and list
         (setf (rest (last list)) list)))
  %%%)

(defutil cycle (:version (1 . 0)
                :depends-on ncycle
                :category lists)
  "Make `list` into a circular list."
  #>%%%>
  (defun cycle (list)
    %%DOC
    (and list
         (ncycle (copy-list list))))
  %%%)

(defutil nest (:version (1 . 0)
               :category lists)
  "Compute a `count` compositions of `function` on `initial-value`."
  #>%%%>
  (defun nest (function initial-value count)
    %%DOC
    (loop
      :repeat count
      :for y := initial-value :then (funcall function y)
      :finally (return y)))
  %%%)

(defutil nest-list (:version (1 . 0)
                    :category lists)
  "Compute a list of `count` compositions of `function` on `initial-value`."
  #>%%%>
  (defun nest-list (function initial-value count)
    %%DOC
    (loop
      :repeat count
      :for y := initial-value :then (funcall function y)
      :collect y))
  %%%)

(defutil safe-nth (:version (1 . 0)
                   :category lists)
  "Find the `n`th element of `list`. If `n` is out of bounds, return
`if-out-of-bounds` (`nil` by default)."
  #>%%%>
  (defun safe-nth (n list &optional if-out-of-bounds)
    %%DOC
    (if (>= n (length list))
        if-out-of-bounds
        (nth n list)))
  %%%)

(defutil mapply (:version (1 . 0)
                 :category lists)
  "Apply `f` to each list of arguments contained within `list` and collect
the results."
  #>%%%>
  (defun mapply (f list)
    %%DOC
    (mapcar #'(lambda (x) (apply f x)) list))
  %%%)

(defutil cartesian-product (:version (1 . 0)
                            :category lists)
  "Compute the cartesian product of `l1` and `l2` as if they were
sets. Optionally, map the function `f` across the product."
  #>%%%>
  (defun cartesian-product (l1 l2 &optional (f 'cl:list))
    %%DOC
    (loop
      :for i :in l1
      :appending (loop
                   :for j :in l2
                   :collecting (funcall f i j))))
  %%%)

;;; TODO: Define a SETF method for END.
(defutil end (:version (1 . 0)
              :category lists)
  "Return the last element of `list` and whether or not it was
  empty."
  #>%%%>
  (defun end (list)
    %%DOC
    (values (car (last list)) (null list)))
  %%%)

(defutil tabulate (:version (1 . 0)
                   :depends-on range
                   :category lists)
  "Return a list evaluations of `f` over the integers `[0,n)`. Mimics the
SML function of the same name."
  #>%%%>
  (defun tabulate (f n)
    %%DOC
    (range 0 n :key f))
  %%%)

(defutil collect-reduce (:version (1 . 0)
                         :category lists)
  "Collects intermediate reduction results of applying `f` to `list`. More
  or less equivalent to `(loop :for i :in list :collect (reduce f i))`."
  #>%%%>
  (defun collect-reduce (f list &key (initial-value (car list) initial-value-p))
    %%DOC
    (loop
      :for j :in (if (not initial-value-p) (cdr list) list)
      :for r := (funcall f initial-value j) :then (funcall f r j)
      :collect r))
  %%%)

(defutil weave (:version (1 . 0)
                :depends-on (flatten-once transpose)
                :category lists)
  "Return a list whose elements alternate between each of the lists
`lists`. Weaving stops when any of the lists has been exhausted."
  #>%%%>
  (defun weave (&rest lists)
    %%DOC
    (flatten-once (transpose lists)))
  %%%)

;;; Author: Paul Khuong (github: pkhuong)
(defutil interleave (:version (1 . 0)
                     :category lists)
  "Return a list whose elements alternate between each of the lists
  `lists`. When a list has been exhausted, interleaving continues with
  whatever other non-empty lists."
  #>%%%>
  (defun interleave (&rest lists)
    %%DOC
    (loop :while (some #'identity lists)
          :nconc (loop :for list-head :on lists
                       :for list := (first list-head)
                       :when list
                         :collect (pop (first list-head)))))
  %%%)

(defutil riffle (:version (1 . 0)
                 :depends-on (weave replicate)
                 :category lists)
  "Insert the item `x` in between each element of `list`."
  #>%%%>
  (defun riffle (list x)
    %%DOC
    (butlast (weave list (replicate (length list) x))))
  %%%)

(defutil extend (:version (1 . 0)
                 :category lists)
  "Adjoin `x` to the end of `xs`."
  #>%%%>
  (defun extend (xs x)
    %%DOC
    (append xs (list x)))
  %%%)

(defutil list-to-vector (:version (1 . 0)
                         :category lists)
  "Convert `list` into a vector."
  #>%%%>
  (defun list-to-vector (list)
    %%DOC
    (make-array (length list) :initial-contents list))
  %%%)

(defutil sequence-to-list (:version (1 . 0)
                           :category lists)
  "Convert the sequence `seq` into a list."
  #>%%%>
  (defun sequence-to-list (seq)
    %%DOC
    (concatenate 'list seq))
  %%%)

(defutil explode (:version (1 . 0)
                  :depends-on sequence-to-list
                  :category lists)
  "The classic `explode` function. Take a string and return a list of
  its characters."
  #>%%%>
  (defun explode (string)
    %%DOC
    (sequence-to-list string))
  %%%)

(defutil implode (:version (1 . 0)
                  :depends-on list-to-vector
                  :category lists)
  "The classic `implode` function. Take a list of characters and return
  the corresponding string."
  #>%%%>
  (defun implode (list-of-characters)
    %%DOC
    (coerce (list-to-vector list-of-characters) 'string))
  %%%)

(defutil inits (:version (1 . 0)
                :category lists)
  "Generate a list of initial sublists of the list `list`. The name
`inits` comes from the Haskell function of the same name.

Example:

    > (inits '(a b c d))
    (NIL (A) (A B) (A B C) (A B C D))"
  #>%%%>
  (defun inits (list)
    %%DOC
    (cons nil (nreverse (maplist #'reverse (reverse list)))))
  %%%)

(defutil tails (:version (1 . 0)
                :category lists)
  "Generate a list of tails of the list `list`. The name `tails`
comes from the Haskell function of the same name.

Example

    > (tails '(a b c d))
    ((A B C D) (B C D) (C D) (D) NIL)"
  ;; This is *almost* equivalent to (maplist #'identity list)
  #>%%%>
  (defun tails (list)
    %%DOC
    (labels ((rec (list acc)
               (if (null list)
                   (nreverse (cons nil acc))
                   (rec (cdr list) (cons list acc)))))
      (rec list nil)))
  %%%)
