(in-package #:quickutil-utilities.utilities)

;; Author: Matt Niemeir (github: m-n)
(defutil syntax-bind (:version (1 . 0)
                      :category (syntax reader))
  "Set `*readtable*` to a copy of `readtable-expression` and modify the copy.
Because `cl:load` and `cl:compile-file` bind `*readtable*`, this effectively gives
a file local readtable in the same way `cl:in-package` gives a file local binding
to `*package*`. Example call:

    (syntax-bind *convenient-readtable*
      (#\\# #\\@) 'send-line-to-channel
      #\\!       'not-reader)

  Sets `#\!` to a nonterminating macro character. Use `(#\!)` to set it to 
  a terminating macro character. A null `readtable-expression` creates a readtable
  with standard Common Lisp syntax then applies the character bindings."
  #>%%%>
  (defmacro syntax-bind (readtable-expression &body characters-functions-plist)
  %%DOC 
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (copy-readtable ,readtable-expression))
    ,@(loop for (chars function) on characters-functions-plist by #'cddr
            collect
                (if (and (consp chars) (cdr chars))
                    `(set-dispatch-macro-character ,(car chars)
                                                   ,(cadr chars)
                                                   ,function)
        `(set-macro-character
           ,(if (consp chars) (car chars) chars)
           ,function
           ,(not (consp chars)))))))
  %%%)
