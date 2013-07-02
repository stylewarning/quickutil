(in-package #:quickutil)

(defutil octet (:version (1 . 0)
                :category (types low-level))
  "A type representing an octet: 8 bits. If a positive integer `n` is
specified, then `(octet n)` represents `8n` bits. This can often be
used for optimization in Common Lisp."
  #>%%%>
  (deftype octet (&optional (n 1))
    %%DOC
    (check-type n (integer 1))
    `(unsigned-byte ,(* 8 n)))
  %%%)

