(in-package #:quickutil)

(defutil alist-to-hash-table (:version (1 . 0)
                              :category hash-tables)
  #1="Create a hash table populated with `kv-pairs`."
  (defun alist-to-hash-table (kv-pairs)
    #1#
    (let ((hashtab (make-hash-table :test #'equal)))
      (loop 
        :for (i j) :in kv-pairs
        :do (setf (gethash i hashtab) j)
        :finally (return hashtab)))))

(defutil hash-table-key-exists-p (:version (1 . 0)
                                  :category (hash-tables orthogonality))
  #1="Does `key` exist in `hash-table`?"
  (defun hash-table-key-exists-p (hash-table key)
    #1#
    (nth-value 1 (gethash key hash-table))))

(defutil dohash (:version (1 . 0)
                 :category (hash-tables orthogonality))
  #1="Iterate over the hash table `table`, executing `body`, with `key` and
   `value` bound to the keys and values of the hash table
   respectively. Return `result` from the iteration form."
  (defmacro dohash ((key value table &optional result) &body body)
    #1#
    `(progn
       (maphash (lambda (,key ,value)
                  ,@body)
                ,table)
       ,result)))
