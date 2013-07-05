(in-package #:quickutil-utilities.utilities)

;; Author: Bike (github: Bike)
(defutil coerce-to-condition (:version (1 . 0)
                              :category (conditions))
  "This function implements the semantics of CL *condition designators*.
It makes a condition, given a `datum` (which may be a
symbol, format control, or condition) and a list of arguments `args`.
See CLHS 9.1.2.1 for more specifics.

`default-type` is the type of objects that should be constructed when
`datum` is a format control.

`supertype` is a type that should be a supertype of the types of all
conditions returned by this function."
  #>%%%>
  (defun coerce-to-condition (datum args default-type supertype)
    %%DOC
    (etypecase datum
      ;; just a symbol, not a class, says 9.1.2.1. why? who knows!
      ;; and of course
      ;;
      ;;     (deftype foo (...args...)
      ;;       ...
      ;;       (find-class 'some-kind-of-condition))
      ;;
      ;;     (error '(foo ...) ...)
      ;;
      ;; is right out.
      (symbol
       (if (subtypep datum supertype)
           (apply #'make-condition datum args)
           (error "~s is not a subclass of ~s, and can't be used as one"
                  datum
                  supertype)))
      
      ;; functions are also format controls.
      ((or function string) (make-condition default-type :format-control datum
                                                         :format-arguments args))
      (condition
       (unless (null args)
         (cerror "Ignore the extra arguments."
                 "Passed a condition to ~s, but passed arguments ~s as well."
                 'coerce-to-condition args))
       datum)))
  %%%)
