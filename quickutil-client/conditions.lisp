(in-package #:quickutil-client)

(define-condition utility-not-found-error (error)
  ((name :initarg :names
         :reader utility-not-found-error-names))
  (:documentation "Error to be invoked when a utility is not found.")
  (:report (lambda (condition stream)
             (format stream "The following utilities were not found:~{ ~S~}."
                     (utility-not-found-error-names condition)))))

(defun utility-not-found-error (util-names)
  "Error when a utilities named UTIL-NAMES are not found."
  (error 'utility-not-found-error :names util-names))
