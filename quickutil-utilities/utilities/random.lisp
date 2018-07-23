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
  "Take N from the sequences of CHOICES sampled according to :WEIGHTS if provided,
otherwise each of CHOICES is equally likely."
  #>%%%>
  (defun sample-from (choices &key (weights nil weights-provided-p) (n 1))
    %%DOC
    (if weights-provided-p
        (let* ((cumm (loop :for i :in weights :for c = i :then (+ c i) :collect c))
               (total (first (last cumm))))
          (loop :repeat n
                :collect (elt choices
                              (find-sorted-position cumm (random total)
                                                    :start 0 :end (1- (length choices))))))
        ;; Uniform sample
        (loop :repeat n :collect (elt choices (random (length choices))))))
  %%%)

