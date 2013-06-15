(in-package #:quickutil)

(defutil stream-direction (:version (1 . 0)
                           :category streams)
  #1="Return the direction of the stream `stream`: `:input`, `:output`, or `:io`."
  (defun stream-direction (stream)
    #1#
    (let ((in? (input-stream-p stream))
          (out? (output-stream-p stream)))
      (cond
        ((and in? out?) :io)
        (in?            :input)
        (out?           :output)))))

(defutil make-null-stream (:version (1 . 0)
                           :provides (make-null-input-stream
                                      make-null-output-stream
                                      make-null-stream)
                           :category streams)
  "Null input and output stream constructors."
  (defun make-null-input-stream ()
    "Make a null input stream."
    (make-concatenated-stream))
  
  (defun make-null-output-stream ()
    "Make a null output stream."
    (make-broadcast-stream))
  
  (defun make-null-stream ()
    "Make a null input/output stream. Acts like the UNIX /dev/null."
    (make-two-way-stream
     (make-null-input-stream)
     (make-null-output-stream))))

(defutil matching-null-stream (:version (1 . 0)
                               :depends-on (stream-direction make-null-stream)
                               :category streams)
  #1="Return a null stream whose direction matches `stream`."
  (defun matching-null-stream (stream)
    #1#
    (case (stream-direction stream)
      ((:io) (make-null-stream))
      ((:input) (make-null-input-stream))
      ((:output) (make-null-output-stream)))))

(defutil with-ignored-streams (:version (1 . 0)
                               :depends-on matching-null-stream
                               :category (io streams))
  (defmacro with-ignored-streams ((&rest streams) &body body)
    (assert (every #'symbolp streams) (streams)
            "WITH-IGNORED-STREAMS can only bind to symbols. ~
             Consider QTL:MAKE-NULL-STREAM instead.")
    `(let* (,@(loop :for stream :in streams
                    :collect (list stream `(matching-null-stream ,stream))))
       ,@body)))

(defutil quietly (:version (1 . 0)
                  :depends-on with-ignored-streams
                  :category io)
  #1="Quietly execute `body`, suppressing all output."
  (defmacro quietly (&body body)
    #1#
    `(with-ignored-streams (*standard-output*
                            *error-output*
                            *debug-io*
                            *trace-output*
                            *terminal-io*)
       ,@body)))
