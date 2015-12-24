(in-package #:quickutil-utilities.utilities)

(defutil true-false (:version (1 . 0)
                     :provides (true false)
                     :category (constants synonyms))
  "`true` and `false` synonyms for `t` and `nil`."
  #>%%%>
  (defconstant true t
    "The true value.")

  (defconstant false nil
    "The false value.")
  %%%)

(defutil yes-no (:version (1 . 0)
                 :provides (yes no)
                 :category (synonyms))
  "Always return `t` or `nil`. Equivalent to `(constantly t)` and `(constantly nil)`."
  #>%%%>
  (defun yes (&rest ignored)
    (declare (ignore ignored))
    t)
  
  (defun no (&rest ignored)
    (declare (ignore ignored))
    nil)
  %%%)
