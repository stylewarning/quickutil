(in-package #:quickutil-utilities.utilities)

(defutil safe-endp (:version (1 . 0)
                    :hidden t
                    :category nil)
  #>%%%>
  (declaim (inline safe-endp))
  (defun safe-endp (x)
    (declare (optimize safety))
    (endp x))
  %%%)

(defutil alist-plist (:version (1 . 0)
                      :depends-on safe-endp
                      :provides (alist-plist plist-alist)
                      :category (alexandria lists alists plists))
  "Convert between alists and plists."
  #>%%%>
  (defun alist-plist (alist)
    "Returns a property list containing the same keys and values as the
association list ALIST in the same order."
    (let (plist)
      (dolist (pair alist)
        (push (car pair) plist)
        (push (cdr pair) plist))
      (nreverse plist)))

  (defun plist-alist (plist)
    "Returns an association list containing the same keys and values as the
property list PLIST in the same order."
    (let (alist)
      (do ((tail plist (cddr tail)))
          ((safe-endp tail) (nreverse alist))
        (push (cons (car tail) (cadr tail)) alist))))
  %%%)

(defutil assoc-value (:version (1 . 0)
                      :depends-on (with-gensyms)
                      :provides (assoc-value rassoc-value)
                      :category (alexandria lists alists))
  "Getters and setters for `assoc` and `rassoc` values."
  #>%%%>
  (declaim (inline racons))
  (defun racons (key value ralist)
    (acons value key ralist))
  
  (macrolet
      ((define-alist-get (name get-entry get-value-from-entry add doc)
         `(progn
            (declaim (inline ,name))
            (defun ,name (alist key &key (test 'eql))
              ,doc
              (let ((entry (,get-entry key alist :test test)))
                (values (,get-value-from-entry entry) entry)))
            (define-setf-expander ,name (place key &key (test ''eql)
                                                   &environment env)
              (multiple-value-bind
                    (temporary-variables initforms newvals setter getter)
                  (get-setf-expansion place env)
                (when (cdr newvals)
                  (error "~A cannot store multiple values in one place" ',name))
                (with-unique-names (new-value key-val test-val alist entry)
                  (values
                   (append temporary-variables
                           (list alist
                                 key-val
                                 test-val
                                 entry))
                   (append initforms
                           (list getter
                                 key
                                 test
                                 `(,',get-entry ,key-val ,alist :test ,test-val)))
                   `(,new-value)
                   `(cond
                      (,entry
                       (setf (,',get-value-from-entry ,entry) ,new-value))
                      (t
                       (let ,newvals
                         (setf ,(first newvals) (,',add ,key ,new-value ,alist))
                         ,setter
                         ,new-value)))
                   `(,',get-value-from-entry ,entry))))))))
    
    (define-alist-get assoc-value assoc cdr acons
      "ASSOC-VALUE is an alist accessor very much like ASSOC, but it can
be used with SETF.")
    
    (define-alist-get rassoc-value rassoc car racons
      "RASSOC-VALUE is an alist accessor very much like RASSOC, but it can
be used with SETF."))
  %%%)

(defutil doplist (:version (1 . 0)
                  :compilation-depends-on (with-gensyms parse-body)
                  :depends-on with-gensyms
                  :category (alexandria lists plists))
  "Iterates over elements of `plist`. `body` can be preceded by
declarations, and is like a `tagbody`. `return` may be used to terminate
the iteration early. If `return` is not used, returns `values`."
  #>%%%>
  (defun malformed-plist (plist)
    (error "Malformed plist: ~S" plist))

  (defmacro doplist ((key val plist &optional values) &body body)
    %%DOC
    (multiple-value-bind (forms declarations) (parse-body body)
      (with-gensyms (tail loop results)
        `(block nil
           (flet ((,results ()
                    (let (,key ,val)
                      (declare (ignorable ,key ,val))
                      (return ,values))))
             (let* ((,tail ,plist)
                    (,key (if ,tail
                              (pop ,tail)
                              (,results)))
                    (,val (if ,tail
                              (pop ,tail)
                              (malformed-plist ',plist))))
               (declare (ignorable ,key ,val))
               ,@declarations
               (tagbody
                  ,loop
                  ,@forms
                  (setf ,key (if ,tail
                                 (pop ,tail)
                                 (,results))
                        ,val (if ,tail
                                 (pop ,tail)
                                 (malformed-plist ',plist)))
                  (go ,loop))))))))
  %%%)

(defutil appendf (:version (1 . 0)
                  :category (alexandria lists))
  "Modify-macro for `append`. Appends `lists` to the place designated by the first
argument."
  #>%%%>
  (define-modify-macro appendf (&rest lists) append
    %%DOC)
  %%%)

(defutil nconcf (:version (1 . 0)
                 :category (alexandria lists))
  "Modify-macro for `nconc`. Concatenates `lists` to place designated by the first
argument."
  #>%%%>
  (define-modify-macro nconcf (&rest lists) nconc
    %%DOC)
  %%%)

(defutil unionf (:version (1 . 0)
                 :category (alexandria lists sets))
  "Modify-macro for `union`. Saves the union of `list` and the contents of the
place designated by the first argument to the designated place."
  #>%%%>
  (define-modify-macro unionf (list &rest args) union
    %%DOC)
  %%%)

(defutil nunionf (:version (1 . 0)
                  :category (alexandria lists sets))
  "Modify-macro for `nunion`. Saves the union of `list` and the contents of the
place designated by the first argument to the designated place. May modify
either argument."
  #>%%%>
  (define-modify-macro nunionf (list &rest args) nunion
    %%DOC)
  %%%)

(defutil reversef (:version (1 . 0)
                   :category (alexandria lists))
  "Modify-macro for `reverse`. Copies and reverses the list stored in the given
place and saves back the result into the place."
  #>%%%>
  (define-modify-macro reversef () reverse
    %%DOC)
  %%%)

(defutil nreversef (:version (1 . 0)
                    :category (alexandria lists))
  "Modify-macro for `nreverse`. Reverses the list stored in the given place by
destructively modifying it and saves back the result into the place."
  #>%%%>
  (define-modify-macro nreversef () nreverse
    %%DOC)
  %%%)

(defutil circular-list (:version (1 . 0)
                        :provides (circular-list
                                   circular-list-p
                                   make-circular-list)
                        :category (alexandria lists types))
  "Creation and detection of circular lists."
  #>%%%>
  (defun circular-list (&rest elements)
    "Creates a circular list of ELEMENTS."
    (let ((cycle (copy-list elements)))
      (nconc cycle cycle)))

  (defun circular-list-p (object)
    "Returns true if OBJECT is a circular list, NIL otherwise."
    (and (listp object)
         (do ((fast object (cddr fast))
              (slow (cons (car object) (cdr object)) (cdr slow)))
             (nil)
           (unless (and (consp fast) (listp (cdr fast)))
             (return nil))
           (when (eq fast slow)
             (return t)))))
  
  (defun make-circular-list (length &key initial-element)
    "Creates a circular list of LENGTH with the given INITIAL-ELEMENT."
    (let ((cycle (make-list length :initial-element initial-element)))
      (nconc cycle cycle)))

  (deftype circular-list ()
    "Type designator for circular lists. Implemented as a SATISFIES type, so not
recommended for performance intensive use. Main usefullness as the
expected-type designator of a TYPE-ERROR."
    `(satisfies circular-list-p))
  %%%)

(defutil circular-tree-p (:version (1 . 0)
                          :category (alexandria trees))
  "Returns true if `object` is a circular tree, `nil` otherwise."
  #>%%%>
  (defun circular-tree-p (object)
    %%DOC
    (labels ((circularp (object seen)
               (and (consp object)
                    (do ((fast (cons (car object) (cdr object)) (cddr fast))
                         (slow object (cdr slow)))
                        (nil)
                      (when (or (eq fast slow) (member slow seen))
                        (return-from circular-tree-p t))
                      (when (or (not (consp fast)) (not (consp (cdr slow))))
                        (return
                          (do ((tail object (cdr tail)))
                              ((not (consp tail))
                               nil)
                            (let ((elt (car tail)))
                              (circularp elt (cons object seen))))))))))
      (circularp object nil)))
  %%%)

(defutil proper-list-p (:version (1 . 0)
                        :category (alexandria lists orthogonality))
  "Returns true if `object` is a proper list."
  #>%%%>
  (defun proper-list-p (object)
    %%DOC
    (cond ((not object)
           t)
          ((consp object)
           (do ((fast object (cddr fast))
                (slow (cons (car object) (cdr object)) (cdr slow)))
               (nil)
             (unless (and (listp fast) (consp (cdr fast)))
               (return (and (listp fast) (not (cdr fast)))))
             (when (eq fast slow)
               (return nil))))
          (t
           nil)))
  %%%)

(defutil proper-list (:version (1 . 0)
                      :depends-on proper-list-p
                      :category (alexandria types lists orthogonality))
  "Type designator for proper lists. Implemented as a `satisfies` type, hence
not recommended for performance intensive use. Main usefulness as a type
designator of the expected type in a `type-error`."
  #>%%%>
  (deftype proper-list ()
    %%DOC
    `(and list (satisfies proper-list-p)))
  %%%)

;; FIXME: these are two different utils...
(defutil proper-list-length/last-car (:version (1 . 0)
                                      :depends-on (safe-endp
                                                   circular-list)
                                      :provides (proper-list-length last-car)
                                      :category (alexandria lists))
  "Compute the length of a proper list, and the last CAR of a list quickly."
  #>%%%>
  (defun circular-list-error (list)
    (error 'type-error
           :datum list
           :expected-type '(and list (not circular-list))))
  
  (macrolet ((def (name lambda-list doc step declare ret1 ret2)
               (assert (member 'list lambda-list))
               `(defun ,name ,lambda-list
                  ,doc
                  (do ((last list fast)
                       (fast list (cddr fast))
                       (slow (cons (car list) (cdr list)) (cdr slow))
                       ,@(when step (list step)))
                      (nil)
                    (declare (dynamic-extent slow) ,@(when declare (list declare))
                             (ignorable last))
                    (when (safe-endp fast)
                      (return ,ret1))
                    (when (safe-endp (cdr fast))
                      (return ,ret2))
                    (when (eq fast slow)
                      (circular-list-error list))))))
    (def proper-list-length (list)
      "Returns length of LIST, signalling an error if it is not a proper list."
      (n 1 (+ n 2))
      ;; KLUDGE: Most implementations don't actually support lists with bignum
      ;; elements -- and this is WAY faster on most implementations then declaring
      ;; N to be an UNSIGNED-BYTE.
      (fixnum n)
      (1- n)
      n)

    (def lastcar (list)
      "Returns the last element of LIST. Signals a type-error if LIST is not a
proper list."
      nil
      nil
      (cadr last)
      (car fast))

    (def (setf lastcar) (object list)
      "Sets the last element of LIST. Signals a type-error if LIST is not a proper
list."
      nil
      nil
      (setf (cadr last) object)
      (setf (car fast) object)))
  %%%)

(defutil ensure-car (:version (1 . 0)
                     :category (alexandria lists conses))
  "If `thing` is a `cons`, its `car` is returned. Otherwise `thing` is returned."
  #>%%%>
  (defun ensure-car (thing)
    %%DOC
    (if (consp thing)
        (car thing)
        thing))
  %%%)

(defutil ensure-cons (:version (1 . 0)
                      :category (alexandria lists conses))
  "If `cons` is a cons, it is returned. Otherwise returns a fresh cons with `cons`
  in the car, and `nil` in the cdr."
  #>%%%>
  (defun ensure-cons (cons)
    %%DOC
    (if (consp cons)
        cons
        (cons cons nil)))
  %%%)

(defutil ensure-list (:version (1 . 0)
                      :category (alexandria lists))
  "If `list` is a list, it is returned. Otherwise returns the list designated by `list`."
  #>%%%>
  (defun ensure-list (list)
    %%DOC
    (if (listp list)
        list
        (list list)))
  %%%)

(defutil remove-from-plist (:version (1 . 0)
                            :provides (remove-from-plist 
                                       delete-from-plist
                                       remove-from-plistf
                                       delete-from-plistf
                                       sans)
                            :category (alexandria lists))
  "Destructive and non-destructive functions to remove items from a
plist, as well as associated modify macros."
  #>%%%>
  (defun remove-from-plist (plist &rest keys)
    "Returns a propery-list with same keys and values as PLIST, except that keys
in the list designated by KEYS and values corresponding to them are removed.
The returned property-list may share structure with the PLIST, but PLIST is
not destructively modified. Keys are compared using EQ."
    (declare (optimize (speed 3)))
    ;; FIXME: possible optimization: (remove-from-plist '(:x 0 :a 1 :b 2) :a)
    ;; could return the tail without consing up a new list.
    (loop for (key . rest) on plist by #'cddr
          do (assert rest () "Expected a proper plist, got ~S" plist)
          unless (member key keys :test #'eq)
            collect key and collect (first rest)))

  (defun delete-from-plist (plist &rest keys)
    "Just like REMOVE-FROM-PLIST, but this version may destructively modify the
provided plist."
    ;; FIXME: should not cons
    (apply 'remove-from-plist plist keys))

  (define-modify-macro remove-from-plistf (&rest keys) remove-from-plist
    "Modify macro for REMOVE-FROM-PLIST.")
  (define-modify-macro delete-from-plistf (&rest keys) delete-from-plist
    "Modify macro for DELETE-FROM-PLIST.")

  (declaim (inline sans))
  (defun sans (plist &rest keys)
    "Alias of REMOVE-FROM-PLIST for backward compatibility."
    (apply #'remove-from-plist plist keys))
  %%%)

(defutil mappend (:version (1 . 0)
                  :category (alexandria lists orthogonality))
  "Applies `function` to respective element(s) of each `list`, appending all the
all the result list to a single list. `function` must return a list."
  #>%%%>
  (defun mappend (function &rest lists)
    %%DOC
    (loop for results in (apply #'mapcar function lists)
          append results))
  %%%)

(defutil setp (:version (1 . 0)
               :category (alexandria lists sets))
  "Returns true if `object` is a list that denotes a set, `nil` otherwise. A list
denotes a set if each element of the list is unique under `key` and `test`."
  #>%%%>
  (defun setp (object &key (test #'eql) (key #'identity))
    %%DOC
    (and (listp object)
         (let (seen)
           (dolist (elt object t)
             (let ((key (funcall key elt)))
               (if (member key seen :test test)
                   (return nil)
                   (push key seen)))))))
  %%%)

(defutil set-equal (:version (1 . 0)
                    :category (alexandria lists sets))
  "Returns true if every element of `list1` matches some element of `list2` and
every element of `list2` matches some element of `list1`. Otherwise returns false."
  #>%%%>
  (defun set-equal (list1 list2 &key (test #'eql) (key nil keyp))
    %%DOC
    (let ((keylist1 (if keyp (mapcar key list1) list1))
          (keylist2 (if keyp (mapcar key list2) list2)))
      (and (dolist (elt keylist1 t)
             (or (member elt keylist2 :test test)
                 (return nil)))
           (dolist (elt keylist2 t)
             (or (member elt keylist1 :test test)
                 (return nil))))))
  %%%)

(defutil map-product (:version (1 . 0)
                      :depends-on (mappend curry ensure-function)
                      :category (alexandria lists))
  "Returns a list containing the results of calling `function` with one argument
from `list`, and one from each of `more-lists` for each combination of arguments.
In other words, returns the product of `list` and `more-lists` using `function`.

Example:

    (map-product 'list '(1 2) '(3 4) '(5 6))
     => ((1 3 5) (1 3 6) (1 4 5) (1 4 6)
         (2 3 5) (2 3 6) (2 4 5) (2 4 6))"
  #>%%%>
  (defun map-product (function list &rest more-lists)
    %%DOC
    (labels ((%map-product (f lists)
               (let ((more (cdr lists))
                     (one (car lists)))
                 (if (not more)
                     (mapcar f one)
                     (mappend (lambda (x)
                                (%map-product (curry f x) more))
                              one)))))
      (%map-product (ensure-function function) (cons list more-lists))))
  %%%)

#+#:ignore
(defun flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))
