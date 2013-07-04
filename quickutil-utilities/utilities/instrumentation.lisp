(in-package #:quickutil-utilities.utilities)

(defutil execution-time (:version (1 . 0)
                         :category instrumentation)
  "Return the number of milliseconds it takes to execute `body`. Also
returns the result as the second value."
  #>%%%>
  (defmacro execution-time (&body body)
    %%DOC
    (let ((tm (gensym))
          (res (gensym)))
      ;; This is coded very particularly to be as accurate as possible.
      `(let* ((,tm (get-internal-real-time))
              (,res (progn ,@body))
              (,tm (floor (* 1000 (- (get-internal-real-time) ,tm))
                          internal-time-units-per-second)))
         (values ,tm ,res))))
  %%%)
