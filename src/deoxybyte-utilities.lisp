;;;
;;; Copyright (C) 2007-2010 Keith James. All rights reserved.
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

;;; Core macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-gensyms ((&rest names) &body body)
    `(let ,(loop for n in names
              collect `(,n (gensym)))
       ,@body)))

(defmacro funcall-if-fn (function arg)
  "If FUNCTION is not null, funcalls FUNCTION on ARG, otherwise
returns ARG."
  `(if ,function
       (funcall ,function ,arg)
     ,arg))

;;; Array copying macro
(defmacro copy-array (source source-start source-end
                      dest dest-start &optional key)
  "Copies elements SOURCE indices SOURCE-START and SOURCE-END to DEST,
inserting them into DEST at DEST-START onwards. If the function KEY is
supplied, it is applied to each element of SOURCE prior to its
insertion into DEST."
  `(loop
      for si of-type vector-index from ,source-start to ,source-end
      for di of-type vector-index = ,dest-start
      then (the vector-index (1+ di))
      do (setf (aref ,dest di) (funcall-if-fn ,key (aref ,source si)))))

(defmacro check-arguments (test-form arguments &optional error-message
                           &rest message-arguments)
  "Checks the validity of ARGUMENTS. If TEST-FORM returns false an
{define-condition invalid-argument-error} is raised. The default error
message may be refined with an additional ERROR-MESSAGE.

Arguments:

- test-form (form): A form to be evaluated. If the form returns NIL,
  an error is raised.
- arguments (list symbols): A list of symbols to which argument values
  are bound.

Optional:

- error-message (string): An error message string.

Rest:

- message-arguments (forms): Forms that evaluate to arguments for the
  error message."
  `(progn
     (unless ,test-form
       (error 'invalid-argument-error
              :parameters ',arguments
              :arguments (list ,@arguments)
              :format-control ,error-message
              :format-arguments (list ,@message-arguments)))
     t))

(defmacro defgenerator (more-form next-form &optional current-form)
  "Returns a generator function that may be passed to any of the
generator utility functions {defun has-more-p} , {defun next}
and {defun current} .

Arguments:

- more-form (form): A form that returns T if the generator can supply
  more values, or NIL otherwise. Used by {defun has-more-p } .

- next-form (form): A form that returns the next value of the
  generator. Used by {defun next} .

Optional:

- current-form (form): A form that returns the current value of the
  generator. Used by {defun current} .

Returns:
- A form that evaluates to an anonymous function."
  (destructuring-bind ((more more-body) (next next-body))
      (list more-form next-form)
    (assert (equal "MORE" (symbol-name more)) ()
            "Expected a MORE form, but found ~a" more-form)
    (assert (equal "NEXT" (symbol-name next)) ()
            "Expected a NEXT form, but found ~a" next-form)
    `(lambda (op)
       (ecase op
         ,@(when current-form
                 (destructuring-bind (current current-body)
                     current-form
                   (assert (equal "CURRENT" (symbol-name current)) ()
                           "Expected a CURRENT form, but found ~a"
                           current-form)
                   `((:current ,current-body))))
         (:more ,more-body)
         (:next ,next-body)))))

;;; Generator utility functions
(defun current (gen)
  "Returns the current value of generator function GEN."
  (declare (optimize (speed 3)))
  (declare (type function gen))
  (funcall gen :current))

(defun next (gen)
  "Returns the next available value from generator function GEN."
  (declare (optimize (speed 3)))
  (declare (type function gen))
  (funcall gen :next))

(defun has-more-p (gen)
  "Returns T if generator function GEN has more available values, or
NIL otherwise."
  (declare (optimize (speed 3)))
  (declare (type function gen))
  (funcall gen :more))

;;; Consumer utility functions
(defun consume (con &rest args)
  "Applies consumer function CON with arguments ARGS."
  (declare (optimize (speed 3)))
  (declare (type function con))
  (apply con args))

(defun collect (gen &optional (n 1))
  "Returns a list of up to N values collected from generator function
GEN. Uses {defun has-more-p} to test the generator and may return an
empty list if no items are available."
  (loop
     repeat n
     while (has-more-p gen)
     collect (next gen)))

(defun discard (gen &optional (n 1))
  "Discards up to N values collected from generator function GEN. Uses
{defun has-more-p} to test the generator and finally returns the
number of values actually discarded."
  (loop
     repeat n
     while (has-more-p gen)
     count (next gen)))

(defun discarding-if (test gen)
  "Returns a new generator function that discards values from
generator function GEN while they satisfy TEST."
  (declare (optimize (speed 3)))
  (declare (type function test))
  (flet ((skip-to-next ()
           (multiple-value-bind (elt found)
               (loop
                  while (has-more-p gen)
                  do (let ((elt (next gen)))
                       (unless (funcall test elt)
                         (return (values elt t)))))
             (values elt found))))
    (multiple-value-bind (elt more)
        (skip-to-next)
      (defgenerator
          (more more)
          (next (prog1
                    elt
                  (multiple-value-setq (elt more)
                    (skip-to-next))))))))
