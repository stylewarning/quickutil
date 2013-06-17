(in-package #:quickutil)

(defutil array-bounds (:version (1 . 0)
                       :provides (array-index array-length)
                       :category (alexandria types arrays cdr))
  "Types related to array bounds."
  #>%%%>
  (deftype array-index (&optional (length array-dimension-limit))
    "Type designator for an index into array of LENGTH: an integer between
0 (inclusive) and LENGTH (exclusive). LENGTH defaults to
ARRAY-DIMENSION-LIMIT."
    `(integer 0 (,length)))

  (deftype array-length (&optional (length array-dimension-limit))
    "Type designator for a dimension of an array of LENGTH: an integer between
0 (inclusive) and LENGTH (inclusive). LENGTH defaults to
ARRAY-DIMENSION-LIMIT."
    `(integer 0 ,length))
  %%%)

(defutil sub-interval-numeric-types (:version (1 . 0)
                                     :compilation-depends-on format-symbol
                                     :provides (negative-double-float
                                                negative-fixnum-p
                                                negative-float
                                                negative-float-p
                                                negative-long-float
                                                negative-long-float-p
                                                negative-rational
                                                negative-rational-p
                                                negative-real
                                                negative-single-float-p
                                                non-negative-double-float
                                                non-negative-double-float-p
                                                non-negative-fixnum
                                                non-negative-fixnum-p
                                                non-negative-float
                                                non-negative-float-p
                                                non-negative-integer-p
                                                non-negative-long-float
                                                non-negative-rational
                                                non-negative-real-p
                                                non-negative-short-float-p
                                                non-negative-single-float
                                                non-negative-single-float-p
                                                non-positive-double-float
                                                non-positive-double-float-p
                                                non-positive-fixnum
                                                non-positive-fixnum-p
                                                non-positive-float
                                                non-positive-float-p
                                                non-positive-integer
                                                non-positive-rational
                                                non-positive-real
                                                non-positive-real-p
                                                non-positive-short-float
                                                non-positive-short-float-p
                                                non-positive-single-float-p
                                                positive-double-float
                                                positive-double-float-p
                                                positive-fixnum
                                                positive-fixnum-p
                                                positive-float
                                                positive-float-p
                                                positive-integer
                                                positive-rational
                                                positive-real
                                                positive-real-p
                                                positive-short-float
                                                positive-short-float-p
                                                positive-single-float
                                                positive-single-float-p
                                                negative-double-float-p
                                                negative-fixnum
                                                negative-integer
                                                negative-integer-p
                                                negative-real-p
                                                negative-short-float
                                                negative-short-float-p
                                                negative-single-float
                                                non-negative-integer
                                                non-negative-long-float-p
                                                non-negative-rational-p
                                                non-negative-real
                                                non-negative-short-float
                                                non-positive-integer-p
                                                non-positive-long-float
                                                non-positive-long-float-p
                                                non-positive-rational-p
                                                non-positive-single-float
                                                positive-integer-p
                                                positive-long-float
                                                positive-long-float-p
                                                positive-rational-p)
               :category (alexandria types cdr))
  "Contains 'sub-interval numeric types'. Majority of the implementation of CDR5."
  ;; This MACROLET will generate most of CDR5 (http://cdr.eurolisp.org/document/5/)
  ;; except the RATIO related definitions and ARRAY-INDEX.
  #>%%%>
  (macrolet
      ((define-sub-interval-type-for (type &optional (base-type type))
         (let ((subtype-names (list))
               (predicate-names (list)))
           (labels ((ensure-car (thing)   ; This is in Alexandria/Quickutil, but
                      (if (consp thing)   ; it is needed at compile time.
                          (car thing)
                          thing))
                    (make-subtype-name (format-control)
                      (let ((result (format-symbol :quickutil format-control
                                                   (symbol-name type))))
                        (push result subtype-names)
                        result))
                    (make-predicate-name (sybtype-name)
                      (let ((result (format-symbol :quickutil '#:~A-p
                                                   (symbol-name sybtype-name))))
                        (push result predicate-names)
                        result))
                    (make-docstring (range-beg range-end range-type)
                      (let ((inf (ecase range-type (:negative "-inf") (:positive "+inf"))))
                        (format nil "Type specifier denoting the ~(~A~) range from ~A to ~A."
                                type
                                (if (equal range-beg ''*) inf (ensure-car range-beg))
                                (if (equal range-end ''*) inf (ensure-car range-end))))))
             (let* ((negative-name     (make-subtype-name '#:negative-~a))
                    (non-positive-name (make-subtype-name '#:non-positive-~a))
                    (non-negative-name (make-subtype-name '#:non-negative-~a))
                    (positive-name     (make-subtype-name '#:positive-~a))
                    (negative-p-name     (make-predicate-name negative-name))
                    (non-positive-p-name (make-predicate-name non-positive-name))
                    (non-negative-p-name (make-predicate-name non-negative-name))
                    (positive-p-name     (make-predicate-name positive-name))
                    (negative-extremum)
                    (positive-extremum)
                    (below-zero)
                    (above-zero)
                    (zero))
               (setf (values negative-extremum below-zero
                             above-zero positive-extremum zero)
                     (ecase type
                       (fixnum       (values 'most-negative-fixnum -1 1 'most-positive-fixnum 0))
                       (integer      (values ''* -1       1        ''* 0))
                       (rational     (values ''* '(0)     '(0)     ''* 0))
                       (real         (values ''* '(0)     '(0)     ''* 0))
                       (float        (values ''* '(0.0E0) '(0.0E0) ''* 0.0E0))
                       (short-float  (values ''* '(0.0S0) '(0.0S0) ''* 0.0S0))
                       (single-float (values ''* '(0.0F0) '(0.0F0) ''* 0.0F0))
                       (double-float (values ''* '(0.0D0) '(0.0D0) ''* 0.0D0))
                       (long-float   (values ''* '(0.0L0) '(0.0L0) ''* 0.0L0))))
               `(progn
                  (deftype ,negative-name ()
                    ,(make-docstring negative-extremum below-zero :negative)
                    `(,',base-type ,,negative-extremum ,',below-zero))

                  (deftype ,non-positive-name ()
                    ,(make-docstring negative-extremum zero :negative)
                    `(,',base-type ,,negative-extremum ,',zero))

                  (deftype ,non-negative-name ()
                    ,(make-docstring zero positive-extremum :positive)
                    `(,',base-type ,',zero ,,positive-extremum))

                  (deftype ,positive-name ()
                    ,(make-docstring above-zero positive-extremum :positive)
                    `(,',base-type ,',above-zero ,,positive-extremum))

                  (declaim (inline ,@predicate-names))

                  (defun ,negative-p-name (n)
                    (and (typep n ',type)
                         (< n ,zero)))

                  (defun ,non-positive-p-name (n)
                    (and (typep n ',type)
                         (<= n ,zero)))

                  (defun ,non-negative-p-name (n)
                    (and (typep n ',type)
                         (<= ,zero n)))

                  (defun ,positive-p-name (n)
                    (and (typep n ',type)
                         (< ,zero n)))))))))
    (define-sub-interval-type-for fixnum integer)
    (define-sub-interval-type-for integer)
    (define-sub-interval-type-for rational)
    (define-sub-interval-type-for real)
    (define-sub-interval-type-for float)
    (define-sub-interval-type-for short-float)
    (define-sub-interval-type-for single-float)
    (define-sub-interval-type-for double-float)
    (define-sub-interval-type-for long-float))
  %%%)

(defutil of-type (:version (1 . 0)
                  :depends-on with-gensyms
                  :category (alexandria))
  "Returns a function of one argument, which returns true when its argument is
of TYPE."
  #>%%%>
  (defun of-type (type)
    %%DOC
    (lambda (thing) (typep thing type)))

  (define-compiler-macro of-type (&whole form type &environment env)
    ;; This can yeild a big benefit, but no point inlining the function
    ;; all over the place if TYPE is not constant.
    (if (constantp type env)
        (with-gensyms (thing)
          `(lambda (,thing)
             (typep ,thing ,type)))
        form))
  %%%)

(defutil type= (:version (1 . 0)
                :category (alexandria types))
  "Returns a primary value of T is TYPE1 and TYPE2 are the same type,
and a secondary value that is true is the type equality could be reliably
determined: primary value of NIL and secondary value of T indicates that the
types are not equivalent."
  #>%%%>
  (declaim (inline type=))
  (defun type= (type1 type2)
    %%DOC
    (multiple-value-bind (sub ok) (subtypep type1 type2)
      (cond ((and ok sub)
             (subtypep type2 type1))
            (ok
             (values nil ok))
            (t
             (multiple-value-bind (sub ok) (subtypep type2 type1)
               (declare (ignore sub))
               (values nil ok))))))
  %%%)

(defutil coercef (:version (1 . 0)
                  :category (alexandria types))
  "Modify-macro for COERCE."
  #>%%%>
  (define-modify-macro coercef (type-spec) coerce
    %%DOC)
  %%%)
