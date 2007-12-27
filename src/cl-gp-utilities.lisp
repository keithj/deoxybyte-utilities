
(in-package :cl-gp-utilities)

(deftype array-index ()
  "Positive array index type for optimizations."
  '(and fixnum (integer 0 *)))


;; Array copying macro

(defmacro copy-array (source source-start source-end
                      dest dest-start &optional key)
  "Copies elements SOURCE indices SOURCE-START and SOURCE-END to DEST,
inserting them into DEST at DEST-START onwards. If the function KEY is
supplied, it is applied to each element of SOURCE prior to its
insertion into DEST."
  `(loop for si of-type array-index from ,source-start to ,source-end
      for di of-type array-index = ,dest-start
      then (the array-index (1+ di))
      do (setf (aref ,dest di) ,(if key
                                    `(funcall ,key (aref ,source si))
                                  `(aref ,source si)))))


;; ASSOCDR and RASSOCAR macros

(defmacro assocdr (key alist &rest args)
  "Returns the cdr of the cons cell returned by calling (assoc KEY
ALIST ARGS)."
 `(cdr (assoc ,key ,alist ,@args)))

(defmacro rassocar (val alist &rest args)
  "Returns the car of the cons cell returned by calling (rassoc KEY
ALIST ARGS)."
  `(car (rassoc ,val ,alist ,@args)))


;; Byte array utility functions

(defun has-byte-at-p (byte-array byte index)
  "Returns T if BYTE-ARRAY has BYTE at INDEX."
  (and (not (zerop (length byte-array)))
       (= byte (aref byte-array index))))

(defun starts-with-byte-p (byte-array byte)
  "Returns T if BYTE-ARRAY has BYTE at index 0."
  (has-byte-at-p byte-array byte 0))

(defun make-sb-string (byte-array &optional (source-start 0) source-end)
  "Returns a new simple-base-string created from the values in
BYTE-ARRAY, a simple-array of (unsigned-byte 8), between indices
SOURCE-START and SOURCE-END, inclusive. SOURCE start defaults to 0 and
SOURCE-END defaults to NIL. The elements of the returned string are
the result of calling code-char on the respective elements of
BYTE-ARRAY."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8)) byte-array))
  (let ((s-end (or source-end (1- (length byte-array)))))
    (declare (type array-index source-start s-end))
    (let ((source-len (length byte-array)))
      (cond ((zerop source-len)
             (make-string 0 :element-type 'base-char))
            ((or (< source-start 0)
                 (> source-start s-end)
                 (>= source-start source-len))
             (error "invalid source-start index ~a for array of length
~a" source-start source-len))
            ((>= s-end source-len)
             (error "invalid source-end index ~a for array of length ~a"
                    s-end source-len))
            (t
             (let ((dest-length (1+ (- s-end source-start))))
               (declare (type array-index dest-length))
               (let ((string (make-string dest-length
                                          :element-type 'base-char)))
                 (copy-array byte-array source-start s-end
                             string 0 #'code-char)
                 string)))))))

(defun concat-into-sb-string (byte-arrays)
  "Returns a new simple-base-string created by concatenating, in the
order supplied, the simple-arrays of (unsigned-byte 8) contained in
the vector BYTE-ARRAYS. The elements of the returned string are the
result of calling code-char on the contents of the respective elements
of BYTE-ARRAYS."
  (let ((string (make-string (reduce #'+ byte-arrays :key #'length)
                             :element-type 'base-char)))
    (loop
       for byte-array of-type (simple-array (unsigned-byte 8))
       across byte-arrays
       for array-length = (length byte-array)
       with offset = 0
       do (unless (zerop array-length)
            (copy-array byte-array 0 (1- array-length)
                        string offset #'code-char)
            (incf offset array-length)))
    string))


;; String utility functions

(defun has-char-at-p (str char index)
  "Returns T if STR has CHAR at INDEX."
  (and (not (zerop (length str)))
       (eql char (char str 0))))

(defun starts-with-char-p (str char)
  "Returns T if STR has CHAR at index 0."
  (has-char-at-p str char 0))

(defun every-char-p (str predicate &rest indices)
  "Applies PREDICATE to characters of string STR indicated by INDICES
and returns T if all those characters match PREDICATE."
  (loop for i in indices
     always (funcall predicate (char str i))))

(defun concat-strings (strs)
  "Returns a new simple-string created by concatenating, in the order
supplied, the simple-strings contained in the vector STRS."
  (declare (optimize (speed 3) (safety 0)))
  (let ((string (make-string (reduce #'+ strs :key #'length)
                             :element-type 'character)))
    (declare (type simple-string string))
    (loop
       for str of-type simple-string across strs
       for str-len = (length str)
       with offset of-type array-index = 0
       do (unless (zerop str-len)
            (copy-array str 0 (1- str-len)
                        string offset) ; copy-array faster than
				       ; replace at (speed 3) (safety
				       ; 0)
            (incf offset str-len)))
    string))

(defun write-wrapped-string (str line-width &optional output-stream)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type simple-string str)
           (type (and fixnum (integer 0 *)) line-width))
  (when (zerop line-width)
    (error "invalid line-with 0"))
  (do* ((write-start 0 (+ line-width write-start))
        (write-end (min line-width (length str))
                   (min (+ write-start line-width) (length str)))
        (line-count 0 (1+ line-count)))
       ((>= write-start (length str)) line-count)
    (write-line str output-stream :start write-start :end write-end)))



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
