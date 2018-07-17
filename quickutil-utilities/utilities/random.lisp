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

(defutil random-from (:version (1 . 0)
                      :depends-on bisect
                      :category random)
  "Take N from CHOICES sampled according to :WEIGHTS if provided,
otherwise each of CHOICES is equally likely."
  #>%%%>
  (defun random-from (choices &key weights (n 1))
    %%DOC
    (let* ((len (length choices))
           (weights (or weights (make-list len :initial-element (float (/ len)))))
           (cumm (loop for i in weights for c = i then (+ c i) collect c))
           (total (first (last cumm))))
      (loop repeat n
            collect (nth (bisect cumm (random total) :low 0 :high (1- len))
                         choices))))
  %%%)

