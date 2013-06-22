(in-package #:quickutil)

(defutil copy-hash-table (:version (1 . 0)
                          :category (alexandria hash-tables orthogonality))
  "Returns a copy of hash table `table`, with the same keys and values
as the `table`. The copy has the same properties as the original, unless
overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, `key`
is invoked on the value. As `key` defaults to `cl:identity`, a shallow
copy is returned by default."
  #>%%%>
  (defun copy-hash-table (table &key key test size
                                     rehash-size rehash-threshold)
    %%DOC
    (setf key (or key 'identity))
    (setf test (or test (hash-table-test table)))
    (setf size (or size (hash-table-size table)))
    (setf rehash-size (or rehash-size (hash-table-rehash-size table)))
    (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
    (let ((copy (make-hash-table :test test :size size
                                 :rehash-size rehash-size
                                 :rehash-threshold rehash-threshold)))
      (maphash (lambda (k v)
                 (setf (gethash k copy) (funcall key v)))
               table)
      copy))
  %%%)

(defutil maphash-keys (:version (1 . 0)
                       :category (alexandria hash-tables))
  "Like `maphash`, but calls `function` with each key in the hash table `table`."
  #>%%%>
  (declaim (inline maphash-keys))
  (defun maphash-keys (function table)
    %%DOC
    (maphash (lambda (k v)
               (declare (ignore v))
               (funcall function k))
             table))
  %%%)

(defutil maphash-values (:version (1 . 0)
                         :category (alexandria hash-tables))
  "Like `maphash`, but calls `function` with each value in the hash table `table`."
  #>%%%>
  (declaim (inline maphash-values))
  (defun maphash-values (function table)
    %%DOC
    (maphash (lambda (k v)
               (declare (ignore k))
               (funcall function v))
             table))
  %%%)

(defutil hash-table-keys (:version (1 . 0)
                          :depends-on maphash-keys
                          :category (alexandria hash-tables))
  "Returns a list containing the keys of hash table `table`."
  #>%%%>
  (defun hash-table-keys (table)
    %%DOC
    (let ((keys nil))
      (maphash-keys (lambda (k)
                      (push k keys))
                    table)
      keys))
  %%%)

(defutil hash-table-values (:version (1 . 0)
                            :depends-on maphash-values
                            :category (alexandria hash-tables))
  "Returns a list containing the values of hash table `table`."
  #>%%%>
  (defun hash-table-values (table)
    %%DOC
    (let ((values nil))
      (maphash-values (lambda (v)
                        (push v values))
                      table)
      values))
  %%%)

(defutil hash-table-alist (:version (1 . 0)
                           :category (alexandria hash-tables))
  "Returns an association list containing the keys and values of hash table
`table`."
  #>%%%>
  (defun hash-table-alist (table)
    %%DOC
    (let ((alist nil))
      (maphash (lambda (k v)
                 (push (cons k v) alist))
               table)
      alist))
  %%%)

(defutil hash-table-plist (:version (1 . 0)
                           :category (alexandria hash-tables))
  "Returns a property list containing the keys and values of hash table
`table`."
  #>%%%>
  (defun hash-table-plist (table)
    %%DOC
    (let ((plist nil))
      (maphash (lambda (k v)
                 (setf plist (list* k v plist)))
               table)
      plist))
  %%%)

(defutil alist-hash-table (:version (1 . 0)
                           :category (alexandria hash-tables))
  "Returns a hash table containing the keys and values of the association list
`alist`. Hash table is initialized using the `hash-table-initargs`."
  #>%%%>
  (defun alist-hash-table (alist &rest hash-table-initargs)
    %%DOC
    (let ((table (apply #'make-hash-table hash-table-initargs)))
      (dolist (cons alist)
        (setf (gethash (car cons) table) (cdr cons)))
      table))
  %%%)

(defutil plist-hash-table (:version (1 . 0)
                           :category (alexandria hash-tables))
  "Returns a hash table containing the keys and values of the property list
`plist`. Hash table is initialized using the `hash-table-initargs`."
  #>%%%>
  (defun plist-hash-table (plist &rest hash-table-initargs)
    %%DOC
    (let ((table (apply #'make-hash-table hash-table-initargs)))
      (do ((tail plist (cddr tail)))
          ((not tail))
        (setf (gethash (car tail) table) (cadr tail)))
      table))
  %%%)

(defutil ensure-gethash (:version (1 . 0)
                         :category (alexandria hash-tables))
  "Like `gethash`, but if `key` is not found in the `hash-table` saves the `default`
under key before returning it. Secondary return value is true if key was
already in the table."
  #>%%%>
  (defmacro ensure-gethash (key hash-table &optional default)
    %%DOC
    `(multiple-value-bind (value ok) (gethash ,key ,hash-table)
       (if ok
           (values value ok)
           (values (setf (gethash ,key ,hash-table) ,default) nil))))
  %%%)
