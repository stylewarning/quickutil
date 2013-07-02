(in-package #:quickutil)

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

