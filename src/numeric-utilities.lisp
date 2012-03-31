;;;
;;; Copyright (c) 2008-2012 Keith James. All rights reserved.
;;;
;;; This file is part of deoxybyte-utilities.
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

(defun iota (count &optional (start 0) (step 1))
  "Generates a list of COUNT integers from START (defaults to 0) with
subsequent integers STEP (defaults to 1) greater than the last, or
less, if a negative STEP is given."
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

(defun number-generator (&optional (start 0) (step 1))
  "Creates a closure of zero arity that returns an integer from START
with subsequent integers incrementing by STEP greater than the last,
or less, is a negative step is given.

Optional:

- start (integer): the start value, defaults to 0.
- step (integer): the step size, defaults to 1.

Returns:

- A closure of zero arity."
  (let ((current start))
    (defgenerator
        (more t)
        (next (prog1
                  current
                (incf current step)))
        (current current))))

(defmacro with-numeric-selector ((name num-bins &key (start 0)
                                       (end (+ start num-bins))
                                       (out-of-bounds :error))
                                 &body body)
  "Defines a local function NAME that accepts a single fixnum
argument and returns an integer between 0 and NUM-BINS, indicating in
which of NUM-BINS sequential, equal-sized bins that value belongs.

Arguments:

- name (symbol): the local function name.
- num-bins (integer): the number of bins into which values are to be
  assigned.

Key:

- start (number): the lower bound of the range of values to be binned,
  defaults to 0.
- end (number): the upper bound of the range of values to be binned.
- oob-error (boolean): if T the function will throw an
  INVALID-ARGUMENT-ERROR when passed values less than START or greater
  than END, otherwise the function will return NIL."
  (with-gensyms (bin-size lower-bound upper-bound)
    `(let ((,lower-bound ,start)
           (,upper-bound (1- ,end))
           (,bin-size (/ (- ,end ,start) ,num-bins)))
      (declare (type fixnum ,bin-size ,lower-bound ,upper-bound))
      (flet ((,name (value)
               (declare (type fixnum value))
               (if (<= ,lower-bound value ,upper-bound)
                   (floor (- value ,lower-bound) ,bin-size)
                 ,(ecase out-of-bounds
                    (:include `(if (< value ,lower-bound)
                                  0
                                ,upper-bound))
                    (:ignore nil)
                    (:error
                     `(error 'invalid-argument-error
                       :parameters 'value
                       :arguments value
                       :format-control "expected a value in the range ~a"
                       :format-arguments (list ,lower-bound
                                               (1- (+ ,lower-bound ,end)))))))))
        ,@body))))

(defmacro define-categorical-binner (value &rest categories)
  "Creates a closure that accepts a single fixnum argument. The argument
value is assigned to the appropriate bin and that bin's total is
incremented. The closure returns two values, the bins as an array of
fixnums and the total number of times values did not fall into any
category. Note that the return values are cumulative across calls
because the counters are closed over.

Arguments:

- value (symbol): a symbol to which values are bound on the subsequent
category forms.

Rest:

- categories (forms): test forms, one for each bin, that evaluate to a
generalized boolean. These forms are evaulated in the order given in
to assign values to bins.

For example, counting numbers into four bins of odd/positive,
odd/negative, even/positive and even/negative.

;;; (define-categorical-binner x
;;;    (and (oddp x) (plusp x))
;;;    (and (oddp x) (minusp x))
;;;    (and (evenp x) (plusp x))
;;;    (and (evenp x) (minusp x)))

Returns:

- A closure accepting a numeric value."
  (let ((num-categories (length categories)))
    `(progn
       (let ((bins (make-array ,num-categories :element-type 'fixnum
                               :initial-element 0))
             (outside 0))
         (lambda (,value)
           (cond ,@(loop
                      for category in categories
                      for i = 0 then (1+ i)
                      collect `(,category
                                (incf (aref bins ,i))))
                 (t
                  (incf outside)))
           (values bins outside))))))

(defun quotedp (form)
  "Returns T if FORM is quoted."
  (and (listp form)
       (eql 'quote (car form))))
