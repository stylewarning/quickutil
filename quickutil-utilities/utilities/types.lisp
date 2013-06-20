(in-package #:quickutil)

(defutil octet (:version (1 . 0)
                :category (types low-level))
  "A type representing an octet: 8 bits. This can often be used for
optimization in Common Lisp."
  #>%%%>
  (deftype octet ()
    %%DOC
    '(unsigned-byte 8))
  %%%)
