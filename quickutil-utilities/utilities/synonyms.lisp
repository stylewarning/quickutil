(in-package #:quickutil-utilities.utilities)

(defutil define-abbreviation (:version (1 . 0)
                              :category (synonyms misc))
  "Abbreviate LONG macro, function, or special operator name as SHORT. If LAMBDA-LIST is present, also copy appropriate SETF-expander."
  #>%%%>
  (defmacro define-abbreviation (short long &optional lambda-list)
    %%DOC
    (if (special-operator-p long)
        `(defmacro ,short (&rest args)
           `(,',long ,@args))
        `(eval-when (:compile-toplevel :execute :load-toplevel)
           (cond
             ((macro-function ',long)
              (setf (macro-function ',short) (macro-function ',long)))
             ((fboundp ',long)
              (setf (fdefinition ',short) (fdefinition ',long))
              ,(when lambda-list
                 `(define-setf-expander ,short ,(append lambda-list)
                    (values ,@(multiple-value-bind
                                    (dummies vals store store-form access-form)
                                  (get-setf-expansion
                                   (cons long (remove-if (lambda (sym)
                                                           (member sym '(&optional &key)))
                                                         lambda-list)))
                                (let ((expansion-vals (mapcar (lambda (x) `(quote ,x))
                                                              (list dummies
                                                                    vals
                                                                    store
                                                                    store-form
                                                                    access-form))))
                                  (setf (second expansion-vals)
                                        (cons 'list vals))
                                  expansion-vals))))))
             (t
              (error "Can't abbreviate ~a" ',long)))
           (setf (documentation ',short 'function) (documentation ',long 'function))
           ',short)))
  %%%
  )

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
