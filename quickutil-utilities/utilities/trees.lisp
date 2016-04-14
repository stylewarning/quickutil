(in-package #:quickutil-utilities.utilities)

(defutil map-tree (:version (1 . 0)
                   :category trees)
  "Map `function` to each of the leave of `tree`."
  #>%%%>
  (defun map-tree (function tree)
    %%DOC
    (check-type tree cons)
    (labels ((rec (tree)
               (cond
                 ((null tree) nil)
                 ((atom tree) (funcall function tree))
                 ((consp tree)
                  (cons (rec (car tree))
                        (rec (cdr tree)))))))
      (rec tree)))
  %%%)

(defutil tree-member-p (:version (1 . 0)
                        :category trees)
  "Returns `t` if `item` is in `tree`, `nil` otherwise."
  #>%%%>
  (defun tree-member-p (item tree &key (test #'eql))
    %%DOC
    (labels ((rec (tree)
               (cond ((null tree) nil)
                     ((atom tree) (funcall test item tree))
                     (t (or (rec (car tree))
                            (rec (cdr tree)))))))
      (rec tree)))
  %%%)

(defutil tree-collect (:version (1 . 0)
                       :category trees)
  "Returns a list of every node in the `tree` that satisfies the `predicate`. If there are any improper lists in the tree, the `predicate` is also applied to their dotted elements."
  #>%%%>
  (defun tree-collect (predicate tree)
    %%DOC
    (let ((sentinel (gensym)))
      (flet ((my-cdr (obj)
               (cond ((consp obj)
                      (let ((result (cdr obj)))
                        (if (listp result)
                            result
                            (list result sentinel))))
                     (t
                      (list sentinel)))))
        (loop :for (item . rest) :on tree :by #'my-cdr
              :until (eq item sentinel)
              :if (funcall predicate item) collect item
                :else
                  :if (listp item)
                    :append (tree-collect predicate item)))))
  %%%)
