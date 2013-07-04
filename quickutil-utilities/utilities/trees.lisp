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
