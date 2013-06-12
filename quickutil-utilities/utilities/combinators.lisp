(in-package #:quickutil)

(defutil compose (:version (1 . 0)
                  :category functional)
  #1="Compose FUNCTIONS right-associatively."
  
  (defun compose (&rest functions)
    #1#
    #'(lambda (x)
        (reduce #'funcall functions
                :initial-value x
                :from-end t)))

  (define-compiler-macro compose (&rest functions)
    "Transform COMPOSE forms into the equivalent lambda function."
    (labels ((sharp-quoted-p (x)
               (and (listp x)
                    (eql (first x) 'function)
                    (symbolp (second x)))))
      `(lambda (x) ,(reduce #'(lambda (fun arg)
                                (if (sharp-quoted-p fun)
                                    (list (second fun) arg)
                                    (list 'funcall fun arg)))
                            functions
                            :initial-value 'x
                            :from-end t)))))

(defutil fix (:version (1 . 0)
              :category functional)
  #1="Apply the fixed-point combinator, also known as the Y-combinator,
to the function F : (A -> B) -> A -> B."
  (defun fix (f)
    #1#
    ((lambda (x) (funcall x x))
     (lambda (x) (funcall f (lambda (y)
                              (funcall (funcall x x) y)))))))
