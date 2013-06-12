(in-package #:quickutil)

(defutil emptyp (:version (1 . 0)
                 :depends-on non-zero-p
                 :category generic)
  #1="Determine if OBJECT is empty."
  (defgeneric emptyp (object)
    (:documentation #1#)
    (:method ((x null)) t)
    (:method ((x list)) (null x))
    (:method ((x vector)) (zerop (length x))) ; STRING :< VECTOR
    (:method ((x array)) (notany #'non-zero-p (array-dimensions x)))
    (:method ((x hash-table)) (zerop (hash-table-count x)))))

(defutil singletonp (:version (1 . 0)
                     :category generic)
  #1="Determine if OBJECT is a singleton object."
  (defgeneric singletonp (object)
    (:documentation #1#)
    (:method ((x list)) (and x (null (cdr x))))
    (:method ((x array)) (every #'(lambda (n) (= 1 n)) (array-dimensions x)))
    (:method ((x sequence)) (= 1 (length x)))
    (:method ((x hash-table)) (= 1 (hash-table-count x)))))
