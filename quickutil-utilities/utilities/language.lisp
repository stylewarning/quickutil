(in-package #:quickutil-utilities.utilities)

(defutil void (:version (1 . 0)
               :category (language misc))
  "Do absolutely nothing, and return absolutely nothing."
  #>%%%>
  (defun void (&rest args)
    %%DOC
    (declare (ignore args))
    (values))
  %%%)

(defutil ensure-boolean (:version (1 . 0)
                         :category (language misc))
  "Convert `x` into a Boolean value."
  #>%%%>
  (defun ensure-boolean (x)
    %%DOC
    (and x t))
  %%%)

(defutil letf* (:version (1 . 0)
                :depends-on (appendf zip)
                :category language)
  "Given a list of `bindings` whose keys are places and whose values are forms, set them for the duration of `body`, but restore their values (as visible upon evaluation of this macro) upon completion. The restoration is ensured with `unwind-protect`."
  #>%%%>
  (defmacro letf* (bindings &body body &environment env)
    %%DOC
    (let (all-dummy-bindings
          all-news all-new-values all-getters all-setters
          gensyms)
      (loop
        :for (place value) :in bindings
        :do (multiple-value-bind
                  (dummy-names dummy-vals news setter getter)
                (get-setf-expansion place env)
              (appendf all-dummy-bindings (zip dummy-names dummy-vals))
              (push (car news) all-news)
              (push value all-new-values)
              (push (gensym (format NIL "old-~A" place)) gensyms)
              (push getter all-getters)
              (push setter all-setters)))
      `(let* ,all-dummy-bindings
         (let ,(zip gensyms all-getters)
           (unwind-protect
                (progn
                  ,@(nreverse (loop :for setter :in all-setters
                                    :for new :in all-news
                                    :for new-value :in all-new-values
                                    :collect `(let ((,new ,new-value))
                                                ,setter)))
                  ,@body)
             ,@(loop :for setter :in all-setters
                     :for new :in all-news
                     :for gensym :in gensyms
                     :collect `(let ((,new ,gensym))
                                 ,setter)))))))
  %%%)

(defutil let1 (:version (1 . 0)
               :category language)
  "Bind VAR to VAL within BODY. Equivalent to LET with one binding."
  #>%%%>
  (defmacro let1 (var val &body body)
    %%DOC
    `(let ((,var ,val))
       ,@body))
  %%%)
