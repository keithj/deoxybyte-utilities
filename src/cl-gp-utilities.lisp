
(in-package :cl-gp-utilities)

(deftype array-subscript ()
  '(and fixnum (integer 0 *)))

;; Array copying macro

(defmacro copy-array (source source-start source-end
                      dest dest-start &optional key)
  "Copies elements SOURCE indices SOURCE-START and SOURCE-END to DEST,
inserting them into DEST at DEST-START onwards. If the function KEY is
supplied, it is applied to each element of SOURCE prior to its
insertion into DEST."
  `(loop for si of-type array-subscript from ,source-start to ,source-end
      for di of-type array-subscript = ,dest-start
      then (the array-subscript (1+ di))
      do (setf (aref ,dest di) ,(if key
                                    `(funcall ,key (aref ,source si))
                                  `(aref ,source si)))))

(defun make-simple-base-string (byte-array)
  "Returns a new simple-base-string of the same length as BYTE-ARRAY,
a simple-array of (unsigned-byte 8). The elements of the returned
string are the result of calling code-char on the respective elements
of BYTE-ARRAY."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8)) byte-array))
  (let ((string (make-string (length byte-array) :element-type 'base-char)))
    (copy-array byte-array 0 (1- (length byte-array))
                string 0 #'code-char)
    string))

;; Introspection utility functions

(defun all-superclasses (class &optional
                         (ceiling (find-class 'standard-object)))
  "Returns a list of all superclasses of CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  #-(or :sbcl :cmu :lispworks)
  (error "ALL-SUPERCLASSES not supported on this implementation.")
  #+:sbcl (set-difference (sb-mop:compute-class-precedence-list class)
                          (sb-mop:compute-class-precedence-list ceiling))
  #+:cmu (set-difference (mop:compute-class-precedence-list class)
                         (mop:compute-class-precedence-list ceiling))
  #+:lispworks (set-difference (clos:class-precedence-list class)
                               (clos:class-precedence-list ceiling)))

(defun all-specialized-methods (class &optional
                                (ceiling (find-class 'standard-object)))
   "Returns a list of all methods specialized on CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
   #-(or :sbcl :cmu :lispworks)
   (error "ALL-SPECIALIZED-METHODS not supported on this
implementation.")
   (apply #'append
         (mapcar #+:sbcl #'sb-mop:specializer-direct-methods
                 #+:cmu #'mop:specializer-direct-methods
                 #+:lispworks #'clos:specializer-direct-methods
                 (all-superclasses class ceiling))))

(defun all-specialized-generic-functions (class &optional
                                          (ceiling (find-class
                                                    'standard-object)))
  "Returns a list of all generic functions specialized on CLASS, up to,
but not including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  #-(or :sbcl :cmu :lispworks)
  (error "ALL-SPECIALIZED-GENERIC-FUNCTIONS not supported on this
implementation.")
  (mapcar #+:sbcl #'sb-mop:method-generic-function
          #+:cmu #'mop:method-generic-function
          #+:lispworks #'clos:method-generic-function
          (all-specialized-methods class ceiling)))

(defun all-external-generic-functions (package)
  "Returns a list of all the external generic functions in PACKAGE."
  (let ((generic-fns))
    (do-external-symbols (s (find-package package) generic-fns)
      (when (fboundp s)
        (let ((fn (symbol-function s)))
          (when (eql 'standard-generic-function (type-of fn))
            (push fn generic-fns)))))
    generic-fns))
