(defutil :foo (:version (1 . 0)
               :depends-on (:bar :quux))
  (defun foo ()
    (+ (bar) (quux))))

(defutil :bar (:version (1 . 0)
               :depends-on (:quux))
  (defun bar ()
    (quux)))

(defutil :quux (:version (1 . 0)
                :depends-on (:baz))
  (defun quux ()
    *baz*))

(defutil :baz (:version (1 . 0)
               :depends-on ())
  (defparameter *baz* 5))

(defutil :eitarow (:version (25 . 0)
                   :depends-on (:robert))
  (defvar *eitarow* *robert*))

(defutil :robert (:version (22 . 0)
                  :depends-on (:eitarow))
  (defvar *robert* *eitarow*))
