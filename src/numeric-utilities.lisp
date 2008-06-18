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

(defun iota (count &optional (start 0) (step 1))
  "Generates a list of COUNT integers from START \(defaults to 0\)
with subsequent integers STEP \(defaults to 1\) greater than the last
\(or less, if a negative STEP is given\)."
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

(defun make-number-gen (&optional (start 0) (step 1))
  "Creates a closure of zero arity that returns an integer from START
with subsequent integers incrementing by STEP greater than the last,
or less, is a negative step is given.

Optional:

- start (integer): the start value, defaults to 0.
- step (integer): the step size, defaults to 1.

Returns:

- A closure of zero arity."
  (let ((current start))
    (lambda (op)
      (ecase op
        (:current current)
        (:next (prog1
                   current
                 (incf current step)))
        (:more t)))))

(defmacro define-numeric-binner (range num-bins &key (start 0)
                                 (lower-bound-test '>=)
                                 (upper-bound-test '<))
  "Creates a closure that accepts a numeric argument. The argument
value is assigned to the appropriate bin and that bin's total is
incremented. The closure returns three values, the bins as an array of
fixnums, the total number of times values fell below the lowest bin
and the total number of times values fell above the highest bin. Note
that the return values are cumulative across calls because the
counters are closed over.

Arguments:

- range (number): the range of values to be recorded
- num-bins (fixnum): the number of bins into which RANGE is divided.

Key:

- start (number): the start of RANGE, defaults to 0.
- lower-bound-test (symbol): the test function for each bin's lower
bound, defaults to '>= .
- upper-bound-test (symbol): the test function for each bin's upper
bound, defaults to '< .

Returns:

- A closure accepting a numeric value."
  (let* ((bin-size (/ range num-bins))
         (bin-ranges (loop
                        for n from 0 below num-bins
                        for offset = start then (+ offset bin-size)
                        collect (list offset (+ offset bin-size))))
         (lower-bound-test (if (quotedp lower-bound-test)
                       (cadr lower-bound-test)
                     lower-bound-test))
         (upper-bound-test (if (quotedp upper-bound-test)
                        (cadr upper-bound-test)
                      upper-bound-test)))
    `(progn
       (let ((bins (make-array ,num-bins :element-type 'fixnum
                               :initial-element 0))
             (outside-low 0)
             (outside-high 0))
         (lambda (value)
           (cond ,@(loop
                      for bin-range in bin-ranges
                      for i = 0 then (1+ i)
                      collect
                        `((and (,lower-bound-test value ,(first bin-range))
                               (,upper-bound-test value ,(second bin-range)))
                          (incf (aref bins ,i))))
                 (t
                  (if (< value ,start)
                      (incf outside-low)
                    (incf outside-high))))
           (values bins outside-low outside-high))))))

(defmacro define-categorical-binner (value &rest categories)
  "Creates a closure that accepts a single argument. The argument
value is assigned to the appropriate bin that bin's total is
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
