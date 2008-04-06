;;;
;;; Copyright (C) 2007-2008, Keith James. All rights reserved.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

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
                                          #\Linefeed #\FormFeed)))
  "Character codes of whitespace characters.")

(deftype array-index ()
  "Positive array index type for optimizations."
  '(and fixnum (integer 0 *)))


;;; Core macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; eval-when to ensure with-gensyms is available at compile-time,
  ;; for use in other macros (i.e. when we expect to use it!)
  (defmacro with-gensyms ((&rest names) &body body)
    `(let ,(loop for n in names
              collect `(,n (gensym)))
       ,@body)))


;;; Array copying macro
(defmacro copy-array (source source-start source-end
                      dest dest-start &optional key)
  "Copies elements SOURCE indices SOURCE-START and SOURCE-END to DEST,
inserting them into DEST at DEST-START onwards. If the function KEY is
supplied, it is applied to each element of SOURCE prior to its
insertion into DEST."
  `(loop
      for si of-type array-index from ,source-start to ,source-end
      for di of-type array-index = ,dest-start
      then (the array-index (1+ di))
      do (setf (aref ,dest di) ,(if key
                                    `(funcall ,key (aref ,source si))
                                  `(aref ,source si)))))


;;; cons utilities
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

(defun splice (list obj n)
  "Splices atom or (a copy of) list OBJ into a copy of LIST at
position N."
  (if (atom obj)
      (nsplice (copy-list list) obj n)
    (nsplice (copy-list list) (copy-list obj) n)))

(defun nsplice (list obj n)
  "Destructively splices atom or list OBJ into LIST at position N."
  (let ((join (nthcdr n list)))
    (if (atom obj)
        (setf (cdr join)
              (cons obj (cdr join)))
      (setf (cdr join)
            (nconc obj (cdr join)))))
  list)
  
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


;;; vector utilties
(defun vector-positions (elt vector &key (start 0) end (test #'eql))
  "Returns a list of indices into VECTOR between START and END where
ELT is present according to TEST (which defaults to EQL)."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type vector vector)
           (type function test))
  (let ((end (or end (length vector))))
    (declare (type array-index start end))
    (unless (<= 0 start end (length vector))
      (error 'invalid-argument-error
             :params '(start end) :args (list start end)
             :text "start must be >= 0 and be <= end"))
    (loop for i from start below end
       when (funcall test elt (aref vector i))
       collect i)))

(defun vector-split-indices (elt vector &key (start 0) end (test #'eql))
  "Returns two values; a list start indices and a list of end indices
into VECTOR between START and END such that if used as start/end
arguments to subseq, VECTOR will be split on ELT. ELT is compared with
elements in VECTOR using TEST, which defaults to EQL."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type vector vector))
  (let ((end (or end (length vector))))
    (declare (type array-index start end))
    (unless (<= 0 start end (length vector))
      (error 'invalid-argument-error
             :params '(start end) :args (list start end)
             :text "start must be >= 0 and be <= end"))
    (let ((positions (vector-positions elt vector
                                       :start start :end end :test test)))
      (if positions
          (loop
             for pos of-type fixnum in positions
             and prev = start then (1+ pos)
             maximize pos into last-pos
             collect prev into starts
             collect pos into ends
             finally (return
                       (values
                        (nconc starts (list (1+ last-pos)))
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
      (error 'invalid-argument-error
             :params '(start end) :args (list start end)
             :text "start must be >= 0 and be <= end"))
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


;;; byte array utility functions
(defun whitespace-byte-p (byte)
  "Returns T if BYTE is one of the currently bound set of whitespace
codes (defaults to codes of #\Space #\Tab #\Return #\Linefeed and
#\FormFeed), or NIL otherwise."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) *whitespace-codes*)
           (type (unsigned-byte 8) byte))
  (loop for w across *whitespace-codes*
     thereis (= w byte)))

(defun whitespace-bytes-p (bytes)
  "Returns T if all the bytes in BYTES are whitespace codes as defined
by WHITESPACE-BYTE-P, or NIL otherwise."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (loop for b across bytes
     always (whitespace-byte-p b)))

(defun content-bytes-p (bytes)
  "Returns T if any of BYTES are not whitespace codes as defined by
WHITESPACE-BYTE-P, or NIL otherwise."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (loop for b across bytes
     thereis (not (whitespace-byte-p b))))

(defun has-byte-at-p (bytes byte index)
  "Returns T if array BYTES has BYTE at INDEX."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) bytes)
           (type (unsigned-byte 8) byte))
  (and (not (zerop (length bytes)))
       (= byte (aref bytes index))))

(defun starts-with-byte-p (bytes byte)
  "Returns T if array BYTES has BYTE at index 0."
  (has-byte-at-p bytes byte 0))

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
                 (>= source-start source-len))
             (error 'invalid-argument-error
                    :params 'source-start
                    :args source-start
                    :text
                    (format nil "source-start must be >= 0 and be <= ~a"
                            source-len)))
            ((> source-start source-end)
             (error 'invalid-argument-error
                    :params '(source-start source-end)
                    :args (list source-start source-end)
                    :text "source-start must be <= source-end"))
            ((>= source-end source-len)
             (error 'invalid-argument-error
                    :params 'source-end
                    :args source-end
                    :text
                    (format nil "source-end must be >= 0 and be <= ~a"
                            source-len)))
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
  (declare (optimize (speed 3) (debug 0)))
  (declare (type vector byte-arrays))
  (let ((new-str (make-string (reduce #'+ byte-arrays :key #'length)
                              :element-type 'base-char))
        (num-arrays (length byte-arrays)))
    (do ((i 0 (1+ i))
         (offset 0))
        ((= i num-arrays) new-str)
      (let ((byte-array (aref byte-arrays i)))
        (declare (type (simple-array (unsigned-byte 8)) byte-array)
                 (type array-index offset))
        (unless (zerop (length byte-array))
          (copy-array byte-array 0 (1- (length byte-array))
                      new-str offset #'code-char)
          (incf offset (length byte-array)))))))

;;; string utility functions
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
  (declare (optimize (speed 3) (debug 0)))
  (declare (type simple-base-string *whitespace-chars*))
  (loop for w across *whitespace-chars*
     thereis (char= w char)))

(defun whitespace-string-p (str)
  "Returns T if all the characters in STR are whitespace as defined by
WHITESPACE-CHAR-P, or NIL otherwise."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type string str))
  (loop for c across str
       always (whitespace-char-p c)))

(defun content-string-p (str)
  "Returns T if any of the characters in STR are not whitespace as
defined by WHITESPACE-CHAR-P, or NIL otherwise."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type string str))
  (loop for c across str
     thereis (not (whitespace-char-p c))))

(defun empty-string-p (str)
  "Returns T if STR is a zero-length string or contains only
whitespace as defined by WHITESPACE-CHAR-P, or NIL otherwise."
  (or (zerop (length str))
      (whitespace-string-p str)))

(defun contains-char-p (str char)
  "Returns T if STR contains CHAR, or NIL otherwise"
  (declare (optimize (speed 3) (debug 0)))
  (declare (type string str))
  (loop for c across str
     thereis (char= char c)))

(defun has-char-at-p (str char index)
  "Returns T if STR has CHAR at INDEX."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type string str))
  (and (not (zerop (length str)))
       (char= char (char str index))))

(defun starts-with-char-p (str char)
  "Returns T if STR has CHAR at index 0."
  (has-char-at-p str char 0))

(defun every-char-p (str test &rest indices)
  "Applies predicate TEST to characters of string STR indicated by
INDICES and returns T if all those characters match TEST."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type string str)
           (type function test))
  (loop for i in indices
     always (funcall test (char str i))))

(defun concat-strings (strs)
  "Returns a new simple-string created by concatenating, in the order
supplied, the simple-strings contained in the vector STRS."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type vector strs))
  (let ((new-str (make-string (reduce #'+ strs :key #'length)
                              :element-type 'character))
        (num-strs (length strs)))
    (do ((i 0 (1+ i))
         (offset 0))
        ((= i num-strs) new-str)
      (let ((str (aref strs i)))
        (declare (type simple-string str)
                 (type array-index offset))
        (unless (zerop (length str))
          (copy-array str 0 (1- (length str))
                      new-str offset)
          (incf offset (length str)))))))

(defun write-wrapped-string (str line-width &optional output-stream)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type simple-string str)
           (type (and fixnum (integer 0 *)) line-width))
  (when (zerop line-width)
    (error "Invalid line-width 0."))
  (do* ((write-start 0 (+ line-width write-start))
        (write-end (min line-width (length str))
                   (min (+ write-start line-width) (length str)))
        (line-count 0 (the fixnum (1+ line-count))))
       ((>= write-start (length str)) line-count)
    (declare (type fixnum line-count))
    (write-line str output-stream :start write-start :end write-end)))

(defun msg (&rest strings)
  (concat-strings
   (coerce (interleave strings (make-string 1 :initial-element #\space))
           'simple-vector)))


;;; Numeric utility functions
(defun iota (count &optional (start 0) (step 1))
  "Generates a list of COUNT integers from START (defaults to 0) with
subsequent integers STEP (defaults to 1) greater than the last (or
less, if a negative STEP is given)."
  (cond ((zerop step)
         (loop repeat count
            collect 0))
        ((minusp step)
         (loop repeat count
            for i downfrom start by (abs step)
            collect i))
        (t
         (loop repeat count
            for i upfrom start by step
            collect i))))


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
