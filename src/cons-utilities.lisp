;;;
;;; Copyright (C) 2008-2009 Keith James. All rights reserved.
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

(in-package :uk.co.deoxybyte-utilities)

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

(defun dotted-pair-p (list)
  "Returns T if LIST is a dotted pair, or NIL otherwise."
  (and (consp list)
       (cdr list)
       (not (consp (cdr list)))))

(defun proper-list-p (list)
  "Returns T if LIST is a proper list, or NIL otherwise. A proper list
is defined as a list whose terminal cdr is NIL. This function does not
test the list for circularity."
  (or (null list)
      (and (consp list)
           (proper-list-p (rest list)))))

(defun splice (list obj n)
  "Splices atom or (a copy of) list OBJ into a copy of LIST at
position N."
  (if (atom obj)
      (nsplice (copy-list list) obj n)
    (nsplice (copy-list list) (copy-list obj) n)))

(defun nsplice (list obj n)
  "Destructively splices atom or list OBJ into LIST at position N."
  (cond ((and (zerop n) (atom obj))
         (cons obj list))
        ((and (zerop n) (listp obj))
         (nconc obj list))
        (t
         (let ((join (nthcdr (1- n) list)))
           (if (atom obj)
               (setf (cdr join)
                     (cons obj (cdr join)))
             (setf (cdr join)
                   (nconc obj (cdr join)))))
         list)))

(defun interleave (list obj)
  "Returns a list containing the members of LIST interleaved with
OBJ."
  (if (endp (rest list))
      list
    (append (list (car list) obj)
            (interleave (rest list) obj))))

(defun flatten (tree)
  "Returns a new list containing the members of TREE."
  (cond ((null tree)
         nil)
        ((atom tree)
         (list tree))
        (t
         (append (flatten (first tree))
                 (flatten (rest tree))))))

(defun exactly-n (predicate n first-seq &rest more-seqs)
  "Analagous to the ANSI standard functions EVERY and SOME, except
that this function returns T if exactly N tests of PREDICATE are true,
or NIL otherwise."
  (loop
     for i from 0 below (apply #'min (length first-seq)
                               (mapcar #'length more-seqs))
     count (apply predicate (elt first-seq i) (loop
                                                 for seq in more-seqs
                                                 collect (elt seq i)))
     into total
     finally (return (= n total))))

(defun exactly-one (predicate first-seq &rest more-seqs)
  "Analagous to the ANSI standard functions EVERY and SOME, except
that this function returns T if exactly one test of PREDICATE is true,
or NIL otherwise."
  (apply #'exactly-n predicate 1 first-seq more-seqs))

(defun collect-key-values (keywords arg-list)
  "For all keywords in list KEYWORDS, finds the keyword and its value
in ARG-LIST, a list containing only keyword and value pairs. Returns
two values, a list of matched keywords and a list of their
corresponding values."
  (loop
     for keyword in arg-list by #'cddr
     for value in (rest arg-list) by #'cddr
     when (member keyword keywords)
     collect keyword into matched-keywords
     and collect value into values
     finally (return (values matched-keywords values))))

(defun key-value (keyword arg-list)
  "Returns the current value of KEYWORD in list ARG-LIST, a list
containing only keyword and value pairs."
  (second (member keyword arg-list)))

(defun remove-key-values (keywords arg-list)
  "Returns two values, a copy of ARG-LIST, a list containing only
keyword and value pairs, with KEYWORDS and their values removed and an
alist containing KEYWORDS and their corresponding values."
  (loop
     for keyword in arg-list by #'cddr
     for value in (rest arg-list) by #'cddr
     if (not (member keyword keywords))
     nconc (list keyword value) into new-arg-list
     else
     collect keyword into removed-keywords
     and collect value into removed-values
     finally (return (values new-arg-list
                             (pairlis removed-keywords
                                      removed-values)))))

(defun modify-key-value (keyword arg-list mod-fn &rest fn-args)
  "Returns a copy of ARG-LIST where the value corresponding to KEYWORD
has been replaced by the result of applying MOD-FN to that
value. MOD-FN must be a function that accepts KEYWORDS's value and may
accept additional arguments supplied in FN-ARGS."
  (loop
     for keyword-n in arg-list by #'cddr
     for value in (rest arg-list) by #'cddr
     if (eql keyword keyword-n)
     nconc (list keyword-n (apply mod-fn value fn-args))
     else
     nconc (list keyword-n value)))

(defun canonical-fn-args (lambda-list fn-args)
  (let* ((key-pos (position '&key lambda-list))
         (fixed-args (subseq fn-args 0 key-pos))
         (keyword-args (subseq fn-args key-pos)))
    (loop
       for key in keyword-args by #'cddr
       for val in (rest keyword-args) by #'cddr
       collect key into keys
       collect (list key val) into key-vals
       finally (return
                 (append fixed-args
                         (mapcan (lambda (key)
                                   (assoc key key-vals))
                                 (sort keys #'string< :key #'symbol-name)))))))
