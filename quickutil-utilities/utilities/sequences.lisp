(in-package #:quickutil)

(defutil sort-copy (:version (1 . 0)
                    :provides (sort-copy stable-sort-copy)
                    :category sequences)
  "Copying versions of SORT and STABLE-SORT."
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
  "Take the first N elements from SEQUENCE."
  #>%%%>
  (defun take (n sequence)
    %%DOC
    (subseq sequence 0 n))
  %%%)

(defutil drop (:version (1 . 0)
               :category sequences)
  "Drop the first N elements from SEQUENCE."
  #>%%%>
  (defun drop (n sequence)
    %%DOC
    ;; This used to be NTHCDR for lists.
    (subseq sequence n))
  %%%)

(defutil subdivide (:version (1 . 0)
                    :category sequences)
  "Split SEQUENCE into subsequences of size CHUNK-SIZE."
  #>%%%>
  (defun subdivide (sequence chunk-size)
    %%DOC
    (check-type sequence sequence)
    (check-type chunk-size (integer 1))
    
    (etypecase sequence
      ;; Since lists have O(N) access time, we iterate through manually,
      ;; collecting each chunk as we pass through it. Using SUBSEQ would
      ;; be O(N^2).
      (list (labels ((rec (sequence acc)
                       (let ((rest (nthcdr chunk-size sequence)))
                         (if (consp rest)
                             (rec rest (cons (subseq sequence 0 chunk-size) acc))
                             (nreverse (cons sequence acc))))))
              (and sequence (rec sequence nil))))
      
      ;; For other sequences like strings or arrays, we can simply chunk
      ;; by repeated SUBSEQs.
      (sequence (loop :with len := (length sequence)
                      :for i :below len :by chunk-size
                      :collect (subseq sequence i (min len (+ chunk-size i)))))))
  %%%)

(defutil n-grams (:version (1 . 0)
                  :depends-on take
                  :category sequences)
  "Find all N-grams of the sequence SEQUENCE."
  #>%%%>
  (defun n-grams (n sequence)
    %%DOC
    (assert (and (plusp n)
                 (<= n (length sequence))))
    
    (etypecase sequence
      ;; Lists
      (list (loop :for i :below (1+ (- (length sequence) n))
                  :for seq := sequence :then (cdr seq)
                  :collect (take n seq)))
      
      ;; General sequences
      (sequence (loop :for i :below (1+ (- (length sequence) n))
                      :collect (subseq sequence i (+ i n))))))
  %%%)
