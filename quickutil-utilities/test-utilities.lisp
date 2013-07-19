(in-package #:cl-user)

(defpackage :quickutil-utilities.test
  (:use #:cl
        #:quickutil-utilities
        #:quickutil-client
        #:quickutil-server
        #:bordeaux-threads)
  (:export #:run-tests))

(in-package :quickutil-utilities.test)

(defun run-tests ()
  (quickutil-server:stop)
  (quickutil-server:start :port 18081)

  ;;(quickutil-client:set-quickutil-host "localhost:18081")

  (loop :with i := 0
        :with log-stream := *standard-output*
        :for name :in (quickutil-utilities:all-utilities)
        :do (progn
              (incf i)
              (format t "~&~%**************************************** Testing ~A:~%" name)
              (defpackage :quickutil (:use :cl) (:nicknames :qtl))
              (let ((*error-output* log-stream)
                    (*standard-output* log-stream))
                (handler-case (quickutil-client:utilize name)
                  (error (e) (princ e log-stream))))
              (delete-package :quickutil))
        :finally (format t "~&~A utilities were tested.~%" i))
  
  (quickutil-server:stop))
