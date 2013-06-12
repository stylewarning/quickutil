(in-package #:quickutil)

(defutil execution-time (:version (1 . 0)
                         :category instrumentation)
  #1="Return the number of milliseconds it takes to execute `body`. Also
returns the result as the second value."
  (defmacro execution-time (&body body)
    #1#
    (let ((tm (gensym))
          (res (gensym)))
      ;; This is coded very particularly to be as accurate as possible.
      `(let* ((,tm (get-internal-real-time))
              (,res (progn ,@body))
              (,tm (floor (* 1000 (- (get-internal-real-time) ,tm))
                          internal-time-units-per-second)))
         (values ,tm ,res)))))
