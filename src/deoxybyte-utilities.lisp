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
      do (setf (aref ,dest di) ,(if key
                                    `(funcall ,key (aref ,source si))
                                  `(aref ,source si)))))

(defmacro defgenerator (&key more next current)
  "Returns a generator function that may be passed to any of the
generator utility functions {defun has-more-p} , {defun next}
and {defun current} .

Key:

- has-more-p (form): A form to be called when passed the function is
  passed to {defun has-more-p} . This should return T if the generator
  can supply more values, or NIL otherwise.
- next (form): A form to be called when passed the function is passed
  to {defun current} . This should return the next value of the
  generator.
- current (form): An optional form to be called when passed the
  function is passed to {defun current} . This should return the
  current value of the generator.

Returns:
- A form that evaluates to an anonymous function."
  (assert more () "A :more form is required.")
  (assert next () "A :next form is required.")
  `(lambda (op)
     (ecase op
       ,@(when current
               `((:current ,current)))
       (:next ,next)
       (:more ,more))))

;;; Generator utility functions
(defun current (gen)
  "Returns the current value of generator function GEN."
  (funcall gen :current))

(defun next (gen)
  "Returns the next available value from generator function GEN."
  (funcall gen :next))

(defun has-more-p (gen)
  "Returns T if generator function GEN has more available values, or
NIL otherwise."
  (funcall gen :more))

;;; Consumer utility functions
(defun consume (con &rest args)
  "Applies consumer function CON with arguments ARGS."
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
          :next (prog1
                    elt
                  (multiple-value-setq (elt more)
                    (skip-to-next)))
          :more more))))
