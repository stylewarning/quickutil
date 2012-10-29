;;;; queue.lisp

(in-package #:quickutil-utilities)

;;; N.B. This is not an efficient queue implementation. DEQUEUE takes
;;; O(N) time. Implementing a heap would be a better idea.

(defstruct queue
  elements)

(defun queue-empty-p (queue)
  (null (queue-elements queue)))

(defun enqueue (queue item)
  (setf (queue-elements queue)
        (push item (queue-elements queue))))

(defun dequeue (queue)
  (let ((elts (queue-elements queue)))
    (prog1 (if (null elts)
               nil
               (first (last elts)))
      (setf (queue-elements queue)
            (butlast elts)))))
