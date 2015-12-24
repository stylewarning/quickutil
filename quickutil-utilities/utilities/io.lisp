(in-package #:quickutil-utilities.utilities)

(defutil stream-direction (:version (1 . 0)
                           :category streams)
  "Return the direction of the stream `stream`: `:input`, `:output`, or `:io`."
  #>%%%>
  (defun stream-direction (stream)
    %%DOC
    (let ((in? (input-stream-p stream))
          (out? (output-stream-p stream)))
      (cond
        ((and in? out?) :io)
        (in?            :input)
        (out?           :output))))
  %%%)

(defutil make-null-stream (:version (1 . 0)
                           :provides (make-null-input-stream
                                      make-null-output-stream
                                      make-null-stream)
                           :category streams)
  "Null input and output stream constructors."
  #>%%%>
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
     (make-null-output-stream)))
  %%%)

(defutil matching-null-stream (:version (1 . 0)
                               :depends-on (stream-direction make-null-stream)
                               :category streams)
  "Return a null stream whose direction matches `stream`."
  #>%%%>
  (defun matching-null-stream (stream)
    %%DOC
    (case (stream-direction stream)
      ((:io) (make-null-stream))
      ((:input) (make-null-input-stream))
      ((:output) (make-null-output-stream))))
  %%%)

(defutil with-ignored-streams (:version (1 . 0)
                               :depends-on matching-null-stream
                               :category (io streams))
  "Ignore any input or output to any of the streams denoted by the
symbols `streams` in the code `body`."
  #>%%%>
  (defmacro with-ignored-streams ((&rest streams) &body body)
    %%DOC
    (assert (every #'symbolp streams) (streams)
            "WITH-IGNORED-STREAMS can only bind to symbols. ~
             Consider QTL:MAKE-NULL-STREAM instead.")
    `(let* (,@(loop :for stream :in streams
                    :collect (list stream `(matching-null-stream ,stream))))
       ,@body))
  %%%)

(defutil quietly (:version (1 . 0)
                  :depends-on with-ignored-streams
                  :category io)
  "Quietly execute `body`, suppressing all output."
  #>%%%>
  (defmacro quietly (&body body)
    %%DOC
    `(with-ignored-streams (*standard-output*
                            *error-output*
                            *debug-io*
                            *trace-output*
                            *terminal-io*)
       ,@body))
  %%%)

(defutil skip-if (:version (1 . 0)
                  :category (io streams))
  "Given a character predicate `char-predicate`, read past all of the characters of the character stream `stream` which satisfy that predicate. Optional arguments `eof-error-p`, `eof-value`, and `recursive-p` are the same as in `read-char`."
  #>%%%>
  (defun skip-if (char-predicate
                  &optional
                    (stream *standard-input*)
                    (eof-error-p T)
                    eof-value
                    recursive-p)
    %%DOC
    (loop :while (funcall char-predicate
                          (peek-char NIL
                                     stream
                                     eof-error-p
                                     eof-value
                                     recursive-p))
          :do (read-char stream eof-error-p eof-value recursive-p)))
  %%%)

(defutil skip (:version (1 . 0)
               :depends-on skip-if
               :category (io streams))
  "Read past all of the characters matching `char` in the character stream `stream`. Optional arguments `eof-error-p`, `eof-value`, and `recursive-p` are the same as in `read-char`."
  #>%%%>
  (defun skip (char
               &optional
                 (stream *standard-input*)
                 (eof-error-p T)
                 eof-value
                 recursive-p)
    %%DOC
    (skip-if (lambda (c) (eql c char))
             stream eof-error-p eof-value recursive-p))
  %%%)
