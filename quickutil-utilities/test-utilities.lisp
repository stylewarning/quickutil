(in-package :cl-user)
(defpackage :quickutil-utilities.test
  (:use :cl
   :quickutil-utilities
   :quickutil-client
   :quickutil-server
   :bordeaux-threads))
(in-package :quickutil-utilities.test)

(quickutil-server:start :port 18081)

(quickutil-client:set-quickutil-host "localhost:18081")

(loop with i = 0
      with log-stream = *standard-output*
      with lock = (bordeaux-threads:make-lock "UTILIZE")
      for name in (quickutil-utilities:all-utilities)
      do
   (incf i)
   (format t "~&Testing for: ~A~%" name)
   (bordeaux-threads:with-lock-held (lock)
    (let ((*error-output* log-stream)
          (*standard-output* log-stream)
          )
      (handler-case (quickutil-client:utilize name)
        (error (e) (princ e log-stream)))))
      finally
      (format t "~&~A utilities were tested.~%" i))

(quickutil-server:stop)
