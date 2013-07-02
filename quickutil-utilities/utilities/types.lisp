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

(defutil multioctet (:version (1 . 0)
                     :category (types low-level))
  "A type representing several octets: `8n` bits for positive integer
`n`. This can often be used for optimization in Common Lisp.

Note the equivalence:

    (multioctet 1) == octet"
  #>%%%>
  (deftype multioctet (n)
    %%DOC
    (check-type n (integer 1))
    `(unsigned-byte ,(* 8 n)))
  %%%)
