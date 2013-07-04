(in-package #:quickutil-utilities.utilities)

(defutil alist-to-hash-table (:version (1 . 0)
                              :category hash-tables)
  "Create a hash table populated with `kv-pairs`."
  #>%%%>
  (defun alist-to-hash-table (kv-pairs)
    %%DOC
    (let ((hashtab (make-hash-table :test #'equal)))
      (loop 
        :for (i j) :in kv-pairs
        :do (setf (gethash i hashtab) j)
        :finally (return hashtab))))
  %%%)

(defutil hash-table-key-exists-p (:version (1 . 0)
                                  :category (hash-tables orthogonality))
  "Does `key` exist in `hash-table`?"
  #>%%%>
  (defun hash-table-key-exists-p (hash-table key)
    %%DOC
    (nth-value 1 (gethash key hash-table)))
  %%%)

(defutil dohash (:version (1 . 0)
                 :category (hash-tables orthogonality))
  "Iterate over the hash table `table`, executing `body`, with `key` and
   `value` bound to the keys and values of the hash table
   respectively. Return `result` from the iteration form."
  #>%%%>
  (defmacro dohash ((key value table &optional result) &body body)
    %%DOC
    `(progn
       (maphash (lambda (,key ,value)
                  ,@body)
                ,table)
       ,result))
  %%%)
