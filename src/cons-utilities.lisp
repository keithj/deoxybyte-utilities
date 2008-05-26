;;;
;;; Copyright (C) 2008 Keith James. All rights reserved.
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
  "Returns two values, a copy of ARGLIST with ARGS and their values
removed and an alist containing ARGS and their corresponding values
and ."
  (loop
     for arg-n in arglist by #'cddr
     for val in (rest arglist) by #'cddr
     if (not (member arg-n args))
     nconc (list arg-n val) into new-arglist
     else
     collect arg-n into removed-args
     and collect val into removed-vals
     finally (return (values new-arglist
                             (pairlis removed-args removed-vals)))))

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
