(in-package #:quickutil-utilities.utilities)

(defutil reseed-random-state (:version (1 . 0)
                              :category random)
  "Reseed the random state use by the random number generator."
  #>%%%>
  (defun reseed-random-state ()
    %%DOC
    (setf *random-state* (make-random-state t)))
  %%%)

(defutil random-between (:version (1 . 0)
                         :category random)
  "Generate a random integer between `a` and `b`, inclusive."
  #>%%%>
  (defun random-between (a b)
    %%DOC
    (assert (>= b a))
    (if (= a b)
        a
        (+ a (random (- (1+ b) a)))))
  %%%)

(defutil sample-from (:version (1 . 0)
                      :depends-on find-sorted-position
                      :category random)
  "Take COUNT from the sequence of CHOICES sampled according to WEIGHTS if provided,
otherwise each of CHOICES is equally likely."
  #>%%%>
  (defun sample-from (choices &key (weights nil weights-provided-p) (count 1))
    %%DOC
    ;; Assert that weights and choices are same length only if weights
    ;; are provided
    (assert (or (not weights-provided-p) (= (length weights) (length choices))))
    (if weights-provided-p
        (let* ((cumm (coerce (loop :for i :in weights :for c := i :then (+ c i) :collect c)
                             'vector))
               (total (aref cumm (1- (length cumm)))))
          (loop :repeat count
                :collect (elt choices
                              (find-sorted-position cumm (random total)))))
        ;; Uniform sample
        (loop :with len := (length choices)
              :repeat count
              :collect (elt choices (random len)))))
  %%%)

