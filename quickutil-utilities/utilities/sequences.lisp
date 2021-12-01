(in-package #:quickutil-utilities.utilities)

(defutil sort-copy (:version (1 . 0)
                    :provides (sort-copy stable-sort-copy)
                    :category sequences)
  "Copying versions of `cl:sort` and `cl:stable-sort`."
  #>%%%>
  (defun sort-copy (sequence predicate &key key)
    "Sort a copy of SEQUENCE according to PREDICATE accessing the
  sequence elements with the function KEY."
    (let ((copy (copy-seq sequence)))
      (sort copy predicate :key key)))

  (defun stable-sort-copy (sequence predicate &key key)
    "Stable sort a copy of SEQUENCE according to PREDICATE accessing the
  sequence elements with the function KEY."
    (let ((copy (copy-seq sequence)))
      (stable-sort copy predicate :key key)))
  %%%)

(defutil take (:version (1 . 0)
               :category sequences)
  "Take the first `n` elements from `sequence`."
  #>%%%>
  (defun take (n sequence)
    %%DOC
    (subseq sequence 0 n))
  %%%)

(defutil drop (:version (1 . 0)
               :category sequences)
  "Drop the first `n` elements from `sequence`."
  #>%%%>
  (defun drop (n sequence)
    %%DOC
    ;; This used to be NTHCDR for lists.
    (subseq sequence n))
  %%%)

(defutil subdivide (:version (1 . 0)
                    :category sequences)
  "Split `sequence` into subsequences of size `chunk-size`."
  #>%%%>
  (defun subdivide (sequence chunk-size)
    %%DOC
    (check-type sequence sequence)
    (check-type chunk-size (integer 1))
    
    (etypecase sequence
      ;; Since lists have O(N) access time, we iterate through manually,
      ;; collecting each chunk as we pass through it. Using SUBSEQ would
      ;; be O(N^2).
      (list (loop :while sequence
                  :collect
                  (loop :repeat chunk-size
                        :while sequence
                        :collect (pop sequence))))
      
      ;; For other sequences like strings or arrays, we can simply chunk
      ;; by repeated SUBSEQs.
      (sequence (loop :with len := (length sequence)
                      :for i :below len :by chunk-size
                      :collect (subseq sequence i (min len (+ chunk-size i)))))))
  %%%)

(defutil n-grams (:version (1 . 0)
                  :depends-on take
                  :category sequences)
  "Find all `n`-grams of the sequence `sequence`."
  #>%%%>
  (defun n-grams (n sequence)
    %%DOC
    (assert (and (plusp n)
                 (<= n (length sequence))))
    
    (etypecase sequence
      ;; Lists
      (list (loop :repeat (1+ (- (length sequence) n))
                  :for seq :on sequence
                  :collect (take n seq)))
      
      ;; General sequences
      (sequence (loop :for i :to (- (length sequence) n)
                      :collect (subseq sequence i (+ i n))))))
  %%%)

(defutil partition-if (:version (1 . 0)
                       :category (sequences functional)
                       :provides (partition-if partition-if-not))
  "Partition sequences based off of a predicate."
  #>%%%>
  (defun partition-if (f seq)
    "Given a predicate F, partition SEQ into two sublists, the first
of which has elements that satisfy F, the second which do not."
    (let ((yes nil)
          (no nil))
      (map nil
           #'(lambda (x)
               (if (funcall f x)
                   (push x yes)
                   (push x no)))
           seq)
      (values yes no)))
  
  (defun partition-if-not (f seq)
    "Partition SEQ into two sublists, the first whose elements do not
satisfy the predicate F, and the second whose elements do."
    (multiple-value-bind (yes no)
        (partition-if f seq)
      (values no yes)))
  %%%)

(defutil equivalence-classes (:version (1 . 0)
                              :category (sequences functional))
  "Partition the sequence `seq` into a list of equivalence classes
defined by the equivalence relation `equiv`."
  #>%%%>
  (defun equivalence-classes (equiv seq)
    %%DOC
    (let ((classes nil))
      (labels ((find-equivalence-class (x)
                 (member-if (lambda (class)
                              (funcall equiv x (car class)))
                            classes))
               
               (add-to-class (x)
                 (let ((class (find-equivalence-class x)))
                   (if class
                       (push x (car class))
                       (push (list x) classes)))))
        (declare (dynamic-extent (function find-equivalence-class)
                                 (function add-to-class))
                 (inline find-equivalence-class
                         add-to-class))
        
        ;; Partition into equivalence classes.
        (map nil #'add-to-class seq)
        
        ;; Return the classes.
        classes)))
  %%%)

(defutil doseq (:version (1 . 0)
                :category sequences)
  "Iterate across the sequence `seq`, binding the variable `var` to
each element of the sequence and executing `body`. Return the value
`return` from the iteration form."
  #>%%%>
  (defmacro doseq ((var seq &optional return) &body body)
    %%DOC
    `(block nil
       (map nil #'(lambda (,var)
                    (tagbody
                       ,@body))
            ,seq)
       ,return))
  %%%)

(defutil find-sorted-position (:version (1 . 0)
                               :category sequences)
  "Find a position in ordered SEQ where ELT would be inserted such
that the result remains ordered according to the binary function
PREDICATE. The keyword arguments :START and :END are used to specify
the region of SEQ in which to search for the position.

Note that the position will be a number X such that all positions in
SEQ before X have (FUNCALL PREDICATE (NTH X SEQ) ELT). This results
in the position being _after_ any elements that match ELT. For example
    (FIND-SORTED-POSITION '(1 2 2 3) 2) => 3."
  #>%%%>
  (defun find-sorted-position (seq elt &key (start 0) (end (length seq)) (predicate #'<))
    %%DOC
    (check-type seq sequence)
    (check-type start unsigned-byte)
    (check-type end unsigned-byte)
    (assert (<= start end) () "The START must be less than the END.")
    (labels ((bisect (start end)
               (let ((mid (floor (+ start end) 2)))
                 (if (= start end)
                     start
                     (if (funcall predicate elt (elt seq mid))
                         (bisect start mid)
                         (bisect (1+ mid) end))))))
      (bisect start end)))
  %%%)
