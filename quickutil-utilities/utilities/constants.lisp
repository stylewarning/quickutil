(in-package #:quickutil-utilities.utilities)

(defutil imaginary-i (:version (1 . 0)
                      :category (math constants)
                      :provides (imaginary-i ii))
  "The imaginary number `i = sqrt(-1)`."
  #>%%%>
  (defconstant imaginary-i #C(0 1) %%DOC)
  (defconstant ii #C(0 1) %%DOC)
  %%%)

(defutil exponential-e (:version (1 . 0)
                        :category (math constants)
                        :provides (exponential-e ee))
  "The exponential number `e = 2.71828...`."
  #>%%%>
  (defconstant exponential-e (exp 1.0d0) %%DOC)
  (defconstant ee (exp 1.0d0) %%DOC)
  %%%)
