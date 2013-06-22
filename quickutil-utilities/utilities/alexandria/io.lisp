;; Copyright (c) 2002-2006, Edward Marco Baringer
;; All rights reserved.

(in-package #:quickutil)

(defutil with-open-file* (:version (1 . 0)
                          :depends-on once-only
                          :category (alexandria files io))
  "Just like `with-open-file`, but `nil` values in the keyword arguments mean to use
the default value specified for `open`."
  #>%%%>
  (defmacro with-open-file* ((stream filespec &key direction element-type
                                                   if-exists if-does-not-exist external-format)
                             &body body)
    %%DOC
    (once-only (direction element-type if-exists if-does-not-exist external-format)
      `(with-open-stream
           (,stream (apply #'open ,filespec
                           (append
                            (when ,direction
                              (list :direction ,direction))
                            (when ,element-type
                              (list :element-type ,element-type))
                            (when ,if-exists
                              (list :if-exists ,if-exists))
                            (when ,if-does-not-exist
                              (list :if-does-not-exist ,if-does-not-exist))
                            (when ,external-format
                              (list :external-format ,external-format)))))
         ,@body)))
  %%%)

(defutil with-input-from-file (:version (1 . 0)
                               :depends-on with-open-file*
                               :category (alexandria files io))
  "Evaluate `body` with `stream-name` to an input stream on the file
`file-name`. `args` is sent as is to the call to `open` except `external-format`,
which is only sent to `with-open-file` when it's not `nil`."
  #>%%%>
  (defmacro with-input-from-file ((stream-name file-name &rest args
                                                         &key (direction nil direction-p)
                                                         &allow-other-keys)
                                  &body body)
    %%DOC
    (declare (ignore direction))
    (when direction-p
      (error "Can't specifiy :DIRECTION for WITH-INPUT-FROM-FILE."))
    `(with-open-file* (,stream-name ,file-name :direction :input ,@args)
       ,@body))
  %%%)

(defutil with-output-to-file (:version (1 . 0)
                              :depends-on with-open-file*
                              :category (alexandria files io))
  "Evaluate `body` with `stream-name` to an output stream on the file
`file-name`. `args` is sent as is to the call to `open` except `external-format`,
which is only sent to `with-open-file` when it's not `nil`."
  #>%%%>
  (defmacro with-output-to-file ((stream-name file-name &rest args
                                                        &key (direction nil direction-p)
                                                        &allow-other-keys)
                                 &body body)
    %%DOC
    (declare (ignore direction))
    (when direction-p
      (error "Can't specifiy :DIRECTION for WITH-OUTPUT-TO-FILE."))
    `(with-open-file* (,stream-name ,file-name :direction :output ,@args)
       ,@body))
  %%%)

(defutil read-file-into-string (:version (1 . 0)
                                :depends-on with-input-from-file
                                :category (alexandria files io strings))
  "Return the contents of the file denoted by `pathname` as a fresh string.

The `external-format` parameter will be passed directly to `with-open-file`
unless it's `nil`, which means the system default."
  #>%%%>
  (defun read-file-into-string (pathname &key (buffer-size 4096) external-format)
    %%DOC
    (with-input-from-file
        (file-stream pathname :external-format external-format)
      (let ((*print-pretty* nil))
        (with-output-to-string (datum)
          (let ((buffer (make-array buffer-size :element-type 'character)))
            (loop
              :for bytes-read = (read-sequence buffer file-stream)
              :do (write-sequence buffer datum :start 0 :end bytes-read)
              :while (= bytes-read buffer-size)))))))
  %%%)

(defutil write-string-into-file (:version (1 . 0)
                                 :depends-on with-output-to-file
                                 :category (alexandria files io strings))
  "Write `string` to `pathname`.

The `external-format` parameter will be passed directly to `with-open-file`
unless it's `nil`, which means the system default."
  #>%%%>
  (defun write-string-into-file (string pathname &key (if-exists :error)
                                                      if-does-not-exist
                                                      external-format)
    %%DOC
    (with-output-to-file (file-stream pathname :if-exists if-exists
                                               :if-does-not-exist if-does-not-exist
                                               :external-format external-format)
      (write-sequence string file-stream)))
  %%%)

(defutil read-file-into-byte-vector (:version (1 . 0)
                                     :depends-on with-input-from-file
                                     :category (alexandria files io))
  "Read `pathname` into a freshly allocated `(unsigned-byte 8)` vector."
  #>%%%>
  (defun read-file-into-byte-vector (pathname)
    %%DOC
    (with-input-from-file (stream pathname :element-type '(unsigned-byte 8))
      (let ((length (file-length stream)))
        (assert length)
        (let ((result (make-array length :element-type '(unsigned-byte 8))))
          (read-sequence result stream)
          result))))
  %%%)

(defutil write-byte-vector-into-file (:version (1 . 0)
                                      :depends-on with-output-to-file
                                      :category (alexandria files io))
  "Write `bytes` to `pathname`."
  #>%%%>
  (defun write-byte-vector-into-file (bytes pathname &key (if-exists :error)
                                                          if-does-not-exist)
    %%DOC
    (check-type bytes (vector (unsigned-byte 8)))
    (with-output-to-file (stream pathname :if-exists if-exists
                                          :if-does-not-exist if-does-not-exist
                                          :element-type '(unsigned-byte 8))
      (write-sequence bytes stream)))
  %%%)

(defutil copy-file (:version (1 . 0)
                    :depends-on (with-input-from-file with-output-to-file copy-stream)
                    :category (alexandria files io orthogonality))
  "Copy a file from `from` to `to`."
  #>%%%>
  (defun copy-file (from to &key (if-to-exists :supersede)
                                 (element-type '(unsigned-byte 8)) finish-output)
    %%DOC
    (with-input-from-file (input from :element-type element-type)
      (with-output-to-file (output to :element-type element-type
                                      :if-exists if-to-exists)
        (copy-stream input output
                     :element-type element-type
                     :finish-output finish-output))))
  %%%)

(defutil copy-stream (:version (1 . 0)
                      :depends-on sub-interval-numeric-types
                      :category (alexandria files io orthogonality))
  "Reads data from `input` and writes it to `output`. Both `input` and `output` must
be streams, they will be passed to `read-sequence` and `write-sequence` and must have
compatible `element-type`s."
  #>%%%>
  (defun copy-stream (input output &key (element-type (stream-element-type input))
                                        (buffer-size 4096)
                                        (buffer (make-array buffer-size
                                                            :element-type element-type))
                                        (start 0) end
                                        finish-output)
    %%DOC
    (check-type start non-negative-integer)
    (check-type end (or null non-negative-integer))
    (check-type buffer-size positive-integer)
    (when (and end
               (< end start))
      (error "END is smaller than START in ~S" 'copy-stream))
    (let ((output-position 0)
          (input-position 0))
      (unless (zerop start)
        ;; FIXME add platform specific optimization to skip seekable streams
        (loop while (< input-position start)
              do (let ((n (read-sequence buffer input
                                         :end (min (length buffer)
                                                   (- start input-position)))))
                   (when (zerop n)
                     (error "~@<Could not read enough bytes from the input to fulfill ~
                           the :START ~S requirement in ~S.~:@>" 'copy-stream start))
                   (incf input-position n))))
      (assert (= input-position start))
      (loop while (or (null end) (< input-position end))
            do (let ((n (read-sequence buffer input
                                       :end (when end
                                              (min (length buffer)
                                                   (- end input-position))))))
                 (when (zerop n)
                   (if end
                       (error "~@<Could not read enough bytes from the input to fulfill ~
                          the :END ~S requirement in ~S.~:@>" 'copy-stream end)
                       (return)))
                 (incf input-position n)
                 (write-sequence buffer output :end n)
                 (incf output-position n)))
      (when finish-output
        (finish-output output))
      output-position))
  %%%)
