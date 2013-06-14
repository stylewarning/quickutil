(in-package #:quickutil)

(defutil string-designator (:version (1 . 0)
                            :category (alexandria types strings))
  #1="A string designator type. A string designator is either a string, a symbol,
or a character."
  (deftype string-designator ()
    #1#
    `(or symbol string character)))
