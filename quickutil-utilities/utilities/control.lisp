(in-package #:quickutil)

(defutil until (:version (1 . 0)
                :category (language control))
  #1="Executes `body` until `expression` is true."
  (defmacro until (expression &body body)
    #1#
    `(do ()
         (,expression)
       ,@body)))

(defutil while (:version (1 . 0)
                :category (language control))
  #1="Executes `body` while `expression` is true."
  (defmacro while (expression &body body)
    #1#
    `(until (not ,expression)
       ,@body)))
