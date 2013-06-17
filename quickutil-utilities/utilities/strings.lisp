(in-package #:quickutil)

(defutil string-append (:version (1 . 0)
                        :provides (string-append string-append*)
                        :category strings)
  "Append strings or a sequence of strings together."
  #>%%%>
  (defun string-append* (sequence-of-strings)
    "Concatenate all of the strings in the sequence
    SEQUENCE-OF-STRINGS."
    (with-output-to-string (*standard-output*)
      (map nil #'write-string sequence-of-strings)))
  
  (defun string-append (&rest strings)
    "Concatenate all of the strings STRINGS together."
    (string-append* strings))
  %%%)

