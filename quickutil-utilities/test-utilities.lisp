(in-package :cl-user)
(defpackage :quickutil-utilities.test
  (:use :cl
   :quickutil-utilities
   :quickutil-client
   :quickutil-server
   :bordeaux-threads))
(in-package :quickutil-utilities.test)

(quickutil-server:stop)
(quickutil-server:start :port 18081)

(quickutil-client:set-quickutil-host "localhost:18081")

#+thread-support
(loop with i = 0
      with log-stream = *standard-output*
      for name in (quickutil-utilities:all-utilities)
      for thread = (bordeaux-threads:make-thread
                    #'(lambda ()
                        (format log-stream "~&Testing for: ~A~%" name)
                        (let ((*error-output* log-stream)
                              (*standard-output* log-stream))
                          (handler-case (quickutil-client:utilize name)
                            (error (e) (princ e log-stream))))))
      do
   (incf i)
   (loop while (bordeaux-threads:thread-alive-p thread)
         do (sleep 0.3))
      finally
      (format t "~&~A utilities were tested.~%" i))

#-thread-support
(error "Your Lisp doesn't support threads.")

(quickutil-server:stop)
