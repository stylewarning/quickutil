(in-package #:quickutil)

(defutil imaginary-i (:version (1 . 0)
                      :category (math constants)
                      :provides (exponential-i ii))
  #1="The imaginary number I = sqrt(-1)."
  (defconstant exponential-i #C(0 1) #1#)
  (defconstant ii #C(0 1) #1#))

(defutil exponential-e (:version (1 . 0)
                        :category (math constants)
                        :provides (exponential-e ee))
  #1="The exponential number E = 2.71828...."
  (defconstant exponential-e (exp 1.0d0) #1#)
  (defconstant ee (exp 1.0d0) #1#))
