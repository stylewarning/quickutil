(in-package #:quickutil)

(defutil required-argument (:version (1 . 0)
                            :category (alexandria conditions))
  "Signals an error for a missing argument of `name`. Intended for
use as an initialization form for structure and class-slots, and
a default value for required keyword arguments."
  #>%%%>
  (defun required-argument (&optional name)
    %%DOC
    (error "Required argument ~@[~S ~]missing." name))
  %%%)

(defutil simple-style-warning (:version (1 . 0)
                               :category (alexandria conditions))
  #>%%%>
  (define-condition simple-style-warning (simple-warning style-warning)
    ())

  (defun simple-style-warning (message &rest args)
    (warn 'simple-style-warning :format-control message :format-arguments args))
  %%%)

;; We don't specify a :report for simple-reader-error to let the
;; underlying implementation report the line and column position for
;; us. Unfortunately this way the message from simple-error is not
;; displayed, unless there's special support for that in the
;; implementation. But even then it's still inspectable from the
;; debugger...
(defutil simple-reader-error (:version (1 . 0)
                              :category (alexandria conditions))
  #>%%%>
  (define-condition simple-reader-error
    #-sbcl(simple-error reader-error)
    #+sbcl(sb-int:simple-reader-error)
    ())

  (defun simple-reader-error (stream message &rest args)
    (error 'simple-reader-error
           :stream stream
           :format-control message
           :format-arguments args))
  %%%)

(defutil simple-parse-error (:version (1 . 0)
                             :category (alexandria conditions))
  #>%%%>
  (define-condition simple-parse-error (simple-error parse-error)
    ())

  (defun simple-parse-error (message &rest args)
    (error 'simple-parse-error
           :format-control message
           :format-arguments args))
  %%%)

(defutil simple-program-error (:version (1 . 0)
                               :category (alexandria conditions))
  #>%%%>
  (define-condition simple-program-error (simple-error program-error)
    ())

  (defun simple-program-error (message &rest args)
    (error 'simple-program-error
           :format-control message
           :format-arguments args))
  %%%)

(defutil ignore-some-conditions (:version (1 . 0)
                                 :category (alexandria conditions))
  "Similar to `cl:ignore-errors` but the (unevaluated) `conditions`
list determines which specific conditions are to be ignored."
  #>%%%>
  (defmacro ignore-some-conditions ((&rest conditions) &body body)
    %%DOC
    `(handler-case
         (progn ,@body)
       ,@(loop for condition in conditions collect
               `(,condition (c) (values nil c)))))
  %%%)

(defutil unwind-protect-case (:version (1 . 0)
                              :category (alexandria conditions))
  "Like `cl:unwind-protect`, but you can specify the circumstances that
the cleanup `clauses` are run.

    clauses ::= (:NORMAL form*)* | (:ABORT form*)* | (:ALWAYS form*)*

Clauses can be given in any order, and more than one clause can be
given for each circumstance. The clauses whose denoted circumstance
occured, are executed in the order the clauses appear.

`abort-flag` is the name of a variable that will be bound to `t` in
`clauses` if the `protected-form` aborted preemptively, and to `nil`
otherwise.

Examples:

    (unwind-protect-case ()
         (protected-form)
       (:normal (format t \"This is only evaluated if PROTECTED-FORM executed normally.~%\"))
       (:abort  (format t \"This is only evaluated if PROTECTED-FORM aborted preemptively.~%\"))
       (:always (format t \"This is evaluated in either case.~%\")))

    (unwind-protect-case (aborted-p)
         (protected-form)
       (:always (perform-cleanup-if aborted-p)))
"
  #>%%%>
  (defmacro unwind-protect-case ((&optional abort-flag) protected-form &body clauses)
    %%DOC
    (check-type abort-flag (or null symbol))
    (let ((gflag (gensym "FLAG+")))
      `(let ((,gflag t))
         (unwind-protect (multiple-value-prog1 ,protected-form (setf ,gflag nil))
           (let ,(and abort-flag `((,abort-flag ,gflag)))
             ,@(loop for (cleanup-kind . forms) in clauses
                     collect (ecase cleanup-kind
                               (:normal `(when (not ,gflag) ,@forms))
                               (:abort  `(when ,gflag ,@forms))
                               (:always `(progn ,@forms)))))))))
  %%%)
