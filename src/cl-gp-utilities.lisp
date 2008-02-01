
(in-package :cl-gp-utilities)

(defvar *whitespace-chars*
  (make-array 5 :element-type 'character
              :initial-contents '(#\Space #\Tab #\Return
                                  #\Linefeed #\FormFeed))
  "Whitespace characters.")

(defvar *whitespace-codes*
  (make-array 5 :element-type '(unsigned-byte 8)
              :initial-contents (mapcar #'char-code
                                        '(#\Space #\Tab #\Return
                                          #\Linefeed #\FormFeed))))
(deftype array-index ()
  "Positive array index type for optimizations."
  '(and fixnum (integer 0 *)))


;;; Array copying macro

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


;;; Cons utilities

(defmacro assocdr (key alist &rest args)
  "Returns the cdr of the cons cell returned by calling (assoc KEY
ALIST ARGS)."
 `(cdr (assoc ,key ,alist ,@args)))

(defmacro rassocar (val alist &rest args)
  "Returns the car of the cons cell returned by calling (rassoc KEY
ALIST ARGS)."
  `(car (rassoc ,val ,alist ,@args)))

(defmacro rplassoc (key alist val &rest args)
  "Performs destructive replacement with VAL (using RPLACD) of the cdr
of the cons returned when calling assoc on ALIST with KEY and ARGS."
`(rplacd (assoc ,key ,alist ,@args) ,val))

(defmacro assocpush (key alist val &rest args)
  "Pushes VAL onto the cdr of the cons returned from ALIST by calling
assoc with KEY and ARGS."
  `(push ,val (assocdr ,key ,alist ,@args)))

(defmacro assocpop (key alist &rest args)
   "Pops a value from the cdr of the cons returned from ALIST by
calling assoc with KEY and ARGS."
   `(pop (assocdr ,key ,alist ,@args)))

(defmacro assocpush+ (key alist val &rest args)
  "Operates as ASSOCPUSH, except that if the cdr to be pushed onto is
an atom, it is first wrapped in a new list."
  (let ((current-val (gensym)))
  `(let ((,current-val (assocdr ,key ,alist ,@args)))
     (if (consp ,current-val)
         (assocpush ,key ,alist ,val ,@args)
       (rplassoc ,key ,alist (list ,val ,current-val) ,@args)))))

(defun interleave (list obj)
  "Returns a list containing the members of LIST interleaved with
OBJ."
  (if (endp (rest list))
      list
    (append (list (car list) obj)
            (interleave (rest list) obj))))

(defun collect-args (args arglist)
  "For all arguments in list ARGS, finds the argument and its value in
list ARGLIST. Returns two values, a list of matched arguments and a
list of their corresponding values."
  (loop
     for arg in arglist by #'cddr
     for val in (rest arglist) by #'cddr
     when (member arg args)
     collect arg into matched-args
     and collect val into vals
     finally (return (values matched-args vals))))

(defun arg-value (arg arglist)
  "Returns the current value of ARG in list ARGLIST."
  (loop
     for arg-n in arglist by #'cddr
     for val in (rest arglist) by #'cddr
     when (eql arg arg-n)
     return val))

(defun remove-args (args arglist)
  "Returns two values, an alist containing ARGS and their
corresponding values and a copy of ARGLIST with ARGS and their values
removed."
  (loop
     for arg-n in arglist by #'cddr
     for val in (rest arglist) by #'cddr
     if (not (member arg-n args))
     nconc (list arg-n val) into new-arglist
     else
     collect arg-n into removed-args
     and collect val into removed-vals
     finally (return (values (pairlis removed-args removed-vals)
                             new-arglist))))

(defun modify-arg (arg arglist mod-fn &rest fn-args)
  "Returns a copy of ARGLIST where the value corresponding to ARG has
been replaced by the result of applying MOD-FN to that value. MOD-FN
must be a function that accepts ARG's value and may accept additional
arguments supplied in FN-ARGS."
  (loop
     for arg-n in arglist by #'cddr
     for val in (rest arglist) by #'cddr
     if (eql arg arg-n)
     nconc (list arg-n (apply mod-fn val fn-args))
     else
     nconc (list arg-n val)))


;;; Vector utilties

(defun vector-positions (elt vector &key (start 0) end (test #'eql))
  "Returns a list of indices into VECTOR between START and END where
ELT is present according to TEST (which defaults to EQL)."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type vector vector)
           (type function test))
  (let ((end (or end (length vector))))
    (declare (type array-index start end))
    (unless (<= 0 start end (length vector))
      (error "Invalid start and end coordinates (~a ~a)." start end))
    (loop for i from start below end
       when (funcall test elt (aref vector i))
       collect i)))

(defun vector-split-indices (elt vector &key (start 0) end (test #'eql))
  "Returns two values; a list start indices and a list of end indices
into VECTOR between START and END such that if used as start/end
arguments to subseq, VECTOR will be split on ELT. ELT is compared with
elements in VECTOR using TEST, which defaults to EQL."
  (declare (optimize (speed 3) (debug 0)))
  (let ((end (or end (length vector))))
    (unless (<= 0 start end (length vector))
      (error "Invalid start and end coordinates (~a ~a)." start end))
    (let ((positions (vector-positions elt vector
                                       :start start :end end :test test)))
      (if positions
          (loop
             for pos in positions
             and prev = start then (1+ pos)
             maximize pos into last-pos
             collect prev into starts
             collect pos into ends
             finally (return (values (nconc starts (list (1+ last-pos)))
                                     (nconc ends (list end)))))
        nil))))

(defun vector-split (elt vector &key (start 0) end (test #'eql)
                     remove-empty-subseqs displace-to-vector)
  "Returns a list of vectors made by splitting VECTOR at ELT, between
START and END. ELT is compared with elements in VECTOR using TEST,
which defaults to EQL. If REMOVE-EMPTY-SUBSEQS is T, any empty
subsequeneces will be omitted from the returned list. If
DISPLACE-TO-VECTOR id T, the returned subsequences will be displaced
to the actual subsequences within VECTOR and will therefore share
structure with VECTOR."
  (let ((end (or end (length vector)))
        (elt-type (array-element-type vector)))
    (unless (<= 0 start end (length vector))
      (error "Invalid start and end coordinates (~a ~a)." start end))
    (multiple-value-bind (starts ends)
        (vector-split-indices elt vector :start start :end end :test test)
      (cond ((and starts ends)
             (loop
                for i in starts
                for j in ends  
                when (not (and remove-empty-subseqs
                               (= i j)))
                collect (if displace-to-vector
                             (make-array (- j i)
                                         :element-type elt-type
                                         :displaced-to vector
                                         :displaced-index-offset i)
                          (subseq vector i j))))
            (displace-to-vector
             (make-array (- end start)
                         :element-type elt-type
                         :displaced-to vector
                         :displaced-index-offset start))
            (t
             (list (subseq vector start end)))))))


;;; Byte array utility functions

(defun whitespace-byte-p (byte)
  "Returns T if BYTE is one of the currently bound set of whitespace
codes (defaults to codes of #\Space #\Tab #\Return #\Linefeed and
#\FormFeed), or NIL otherwise."
  (loop for w across *whitespace-codes*
     thereis (= w byte)))

(defun whitespace-bytes-p (bytes)
  "Returns T if all the bytes in BYTES are whitespace code as defined
by WHITESPACE-BYTE-P, or NIL otherwise."
  (loop for b across bytes
       always (whitespace-byte-p b)))

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
  (declare (optimize (speed 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) byte-array))
  (let ((source-end (or source-end (1- (length byte-array)))))
    (declare (type array-index source-start source-end))
    (let ((source-len (length byte-array)))
      (cond ((zerop source-len)
             (make-string 0 :element-type 'base-char))
            ((or (< source-start 0)
                 (> source-start source-end)
                 (>= source-start source-len))
             (error "Invalid source-start index ~a for array of length ~a."
                    source-start source-len))
            ((>= source-end source-len)
             (error "Invalid source-end index ~a for array of length ~a."
                    source-end source-len))
            (t
             (let ((dest-length (1+ (- source-end source-start))))
               (declare (type array-index dest-length))
               (let ((string (make-string dest-length
                                          :element-type 'base-char)))
                 (copy-array byte-array source-start source-end
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


;;; String utility functions

(defun control-char-p (char)
  "Returns T if CHAR is an ASCII control character (all characters
with codes 0-31, inclusive, and the character with code 127), or NIL
otherwise."
  (and (< 31 (char-code char))
       (\= 127 (char-code char))))

(defun whitespace-char-p (char)
  "Returns T if CHAR is one of the currently bound set of whitespace
characters (defaults to #\Space #\Tab #\Return #\Linefeed and
#\FormFeed), or NIL otherwise."
  (loop for w across *whitespace-chars*
     thereis (char= w char)))

(defun whitespace-string-p (str)
  "Returns T if all the characters in STR are whitespace as defined by
WHITESPACE-CHAR-P, or NIL otherwise."
  (loop for c across str
       always (whitespace-char-p c)))

(defun has-char-at-p (str char index)
  "Returns T if STR has CHAR at INDEX."
  (and (not (zerop (length str)))
       (char= char (char str index))))

(defun starts-with-char-p (str char)
  "Returns T if STR has CHAR at index 0."
  (has-char-at-p str char 0))

(defun every-char-p (str test &rest indices)
  "Applies predicate TEST to characters of string STR indicated by
INDICES and returns T if all those characters match TEST."
  (loop for i in indices
     always (funcall test (char str i))))

(defun concat-strings (strs)
  "Returns a new simple-string created by concatenating, in the order
supplied, the simple-strings contained in the vector STRS."
  (declare (optimize (speed 3) (debug 0)))
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
  (declare (optimize (speed 3) (debug 0)))
  (declare (type simple-string str)
           (type (and fixnum (integer 0 *)) line-width))
  (when (zerop line-width)
    (error "Invalid line-with 0."))
  (do* ((write-start 0 (+ line-width write-start))
        (write-end (min line-width (length str))
                   (min (+ write-start line-width) (length str)))
        (line-count 0 (1+ line-count)))
       ((>= write-start (length str)) line-count)
    (write-line str output-stream :start write-start :end write-end)))


;;; Condition utility functions

(defun msg (&rest strings)
  (concat-strings
   (coerce (interleave strings (make-string 1 :initial-element #\space))
           'simple-vector)))



;;; Introspection utility functions

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
   (error msg("ALL-SPECIALIZED-METHODS not supported"
              "on this implementation."))
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
  (error msg("ALL-SPECIALIZED-GENERIC-FUNCTIONS not supported"
             "on this implementation."))
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
