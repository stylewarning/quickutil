;;;; Test utilities

;;; Fake utilities

(defutil :foo (:version (1 . 0)
               :depends-on (:bar :quux))
  (defun foo ()
    (+ (bar) (quux))))

(defutil :bar (:version (1 . 0)
               :depends-on (:quux))
  (defun bar ()
    (quux)))

(defutil :quux (:version (1 . 0)
                :depends-on (:baz :even?))
  (defun quux ()
    (even? *baz*)))

(defutil :baz (:version (1 . 0)
               :depends-on ())
  (defparameter *baz* 5))

(defutil :eitarow (:version (25 . 0)
                   :depends-on (:robert))
  (defvar *eitarow* *robert*))

(defutil :robert (:version (22 . 0)
                  :depends-on (:eitarow))
  (defvar *robert* *eitarow*))


;;; Real utilities, taken from QTILITY.

(defutil :non-negative-p (:version (1 . 0))
  (defun non-negative-p (n)
    "Check if N is non-negative."
    (declare (type real n))
    (>= n 0)))

(defutil :range (:version (1 . 0))
  (defun range (start end &key (step 1) (key 'identity))
    "Return the list of numbers n such that START <= n < END and n =
START + k*STEP. If a function KEY is provided, then apply KEY to each
number."
    (assert (<= start end))
    (loop :for i :from start :below end :by step :collecting (funcall key i))))

(defutil :iota (:version (1 . 0)
                :depends-on (:range :non-negative-p))
  (defun iota (n)
    "Return [0, ..., N-1]."
    (assert (non-negative-p n))
    (range 0 n)))

(defutil :iota+1 (:version (1 . 0)
                  :depends-on (:range))
  (defun iota+1 (n)
    "Return [1, ..., N]."
    (assert (>= n 1))
    (range 1 (1+ n))))


;;; Circular dependency

(defutil :even? (:version (1 . 0)
                 :depends-on (:odd?))
  (defun even? (n)
    (if (plusp n)
        (odd? (1- n))
        t)))

(defutil :odd? (:version (1 . 0)
                 :depends-on (:even?))
  (defun odd? (n)
    (if (plusp n)
        (even? (1- n))
        nil)))