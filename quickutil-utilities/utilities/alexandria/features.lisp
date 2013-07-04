(in-package #:quickutil-utilities.utilities)

(defutil featurep (:version (1 . 0)
                   :depends-on switch
                   :category (alexandria features))
  "Returns `t` if the argument matches the state of the `*features*`
list and `nil` if it does not. `feature-expression` can be any atom
or list acceptable to the reader macros `#+` and `#-`."
  #>%%%>
  (defun featurep (feature-expression)
    %%DOC
    (etypecase feature-expression
      (symbol (not (null (member feature-expression *features*))))
      (cons (check-type (first feature-expression) symbol)
       (eswitch ((first feature-expression) :test 'string=)
         (:and (every #'featurep (rest feature-expression)))
         (:or  (some #'featurep (rest feature-expression)))
         (:not (assert (= 2 (length feature-expression)))
               (not (featurep (second feature-expression))))))))
  %%%)
