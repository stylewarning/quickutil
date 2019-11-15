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

(defutil tree-collect (:version (1 . 1)
                       :category trees)
  "Returns a list of every node in the `tree` that satisfies the `predicate`. If there are any improper lists in the tree, the `predicate` is also applied to their dotted elements."
  #>%%%>
  (defun tree-collect (predicate tree)
    %%DOC
    (loop for (first . rest) on tree
       if (funcall predicate first)
       collect first into result
       else if (consp first)
       append (tree-collect predicate first) into result
       unless (consp rest) return (if (and rest (funcall predicate rest))
				      (append result (list rest))
				      result)))
  %%%)
