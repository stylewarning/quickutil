(in-package #:quickutil)

(defutil imaginary-i (:version (1 . 0)
                      :category (math constants)
                      :provides (imaginary-i ii))
  #1="The imaginary number `i = sqrt(-1)`."
  (defconstant imaginary-i #C(0 1) #1#)
  (defconstant ii #C(0 1) #1#))

(defutil exponential-e (:version (1 . 0)
                        :category (math constants)
                        :provides (exponential-e ee))
  #1="The exponential number `e = 2.71828...`."
  (defconstant exponential-e (exp 1.0d0) #1#)
  (defconstant ee (exp 1.0d0) #1#))
