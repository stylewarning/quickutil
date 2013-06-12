(in-package #:quickutil)

(defutil map-tree (:version (1 . 0)
                   :category trees)
  #1="Map `function` to each of the leave of `tree`."
  (defun map-tree (function tree)
    #1#
    (check-type tree cons)
    (labels ((rec (tree)
               (cond
                 ((null tree) nil)
                 ((atom tree) (funcall function tree))
                 ((consp tree)
                  (cons (rec (car tree))
                        (rec (cdr tree)))))))
      (rec tree))))

(defutil tree-member-p (:version (1 . 0)
                        :category trees)
  #1="Returns `t` if `item` is in `tree`, `nil` otherwise."
  (defun tree-member-p (item tree &key (test #'eql))
    #1#
    (labels ((rec (tree)
               (cond ((null tree) nil)
                     ((atom tree) (funcall test item tree))
                     (t (or (rec (car tree))
                            (rec (cdr tree)))))))
      (rec tree))))
