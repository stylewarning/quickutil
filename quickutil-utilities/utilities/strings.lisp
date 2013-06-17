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

(defutil separated-string-append (:version (1 . 0)
                                  :provides (separated-string-append
                                             separated-string-append*)
                                  :category strings)
  "Append strings or a sequence of strings, separated by a separator."
  #>%%%>
  (defun separated-string-append* (separator sequence-of-strings)
    "Concatenate all of the strings in SEQUENCE-OF-STRINGS separated
    by the string SEPARATOR."
    (etypecase sequence-of-strings
      (null "")
      
      (cons (with-output-to-string (*standard-output*)
              (mapl #'(lambda (tail)
                        (write-string (car tail))
                        (unless (null (cdr tail))
                          (write-string separator)))
                    sequence-of-strings)))
      
      (sequence
       (let ((length (length sequence-of-strings)))
         (with-output-to-string (*standard-output*)
           (map nil #'(lambda (string)
                        (write-string string)
                        (unless (zerop (decf length))
                          (write-string separator)))
                sequence-of-strings))))))
  
  (defun separated-string-append (separator &rest strings)
    "Concatenate the strings STRINGS separated by the string
SEPARATOR."
    (separated-string-append* separator strings))
  %%%)

(defutil string-append-space (:version (1 . 0)
                              :depends-on separated-string-append
                              :category strings)
  "Concatenate `strings` separated by a single space."
  #>%%%>
  (defun string-append-space (&rest strings)
    %%DOC
    (separated-string-append* " " strings))
  %%%)
