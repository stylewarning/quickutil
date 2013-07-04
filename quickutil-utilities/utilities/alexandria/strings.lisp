(in-package #:quickutil-utilities.utilities)

(defutil string-designator (:version (1 . 0)
                            :category (alexandria types strings))
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  #>%%%>
  (deftype string-designator ()
    %%DOC
    `(or symbol string character))
  %%%)
