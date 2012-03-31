;;;
;;; Copyright (c) 2010-2012 Keith James. All rights reserved.
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

(in-package :uk.co.deoxybyte-utilities-test)

(defun make-random-list (max-length elt-fn &rest elt-args)
  (loop
     repeat (random max-length)
     collect (apply elt-fn elt-args)))

(defun make-list-of-lists (max-length)
  (make-random-list max-length (lambda ()
                                 (list (random 1000)))))

(defun test-numlist (fn1 fn2)
  (loop
     repeat 100
     always (let ((x (make-random-list 10000 #'random 1000))
                  (y (make-random-list 10000 #'random 1000)))
              (equal (funcall fn1 x y) (funcall fn2 x y)))))

(defun test-list-of-lists (fn1 fn2)
  (loop
     repeat 100
     always (let ((x (make-random-list 1000 #'make-list-of-lists 100))
                  (y (make-random-list 1000 #'make-list-of-lists 100)))
              (equal (funcall fn1 x y :key #'car)
                     (funcall fn2 x y :key #'car)))))

(addtest (deoxybyte-utilities-tests) linear-union/1
  (test-numlist #'union #'linear-union))
    
(addtest (deoxybyte-utilities-tests) linear-union/2
  (test-list-of-lists #'union #'linear-union))

(addtest (deoxybyte-utilities-tests) linear-intersection/1
  (test-numlist #'intersection #'linear-intersection))
    
(addtest (deoxybyte-utilities-tests) linear-intersection/2
  (test-list-of-lists #'intersection #'linear-intersection))

(addtest (deoxybyte-utilities-tests) linear-set-difference/1
  (test-numlist #'set-difference #'linear-set-difference))
    
(addtest (deoxybyte-utilities-tests) linear-set-difference/2
  (test-list-of-lists #'set-difference #'linear-set-difference))

(addtest (deoxybyte-utilities-tests) linear-set-exclusive-or/1
  (test-numlist #'set-exclusive-or #'linear-set-exclusive-or))
    
(addtest (deoxybyte-utilities-tests) linear-set-exclusive-or/2
  (test-list-of-lists #'set-exclusive-or #'linear-set-exclusive-or))

(addtest (deoxybyte-utilities-tests) linear-subsetp/1
  (test-numlist #'subsetp #'linear-subsetp))
    
(addtest (deoxybyte-utilities-tests) linear-subsetp/2
  (test-list-of-lists #'subsetp #'linear-subsetp))

(addtest (deoxybyte-utilities-tests) linear-set-equal/1
  (test-numlist (lambda (x y &rest args)
                  (declare (ignore args))
                  (equal x y)) #'linear-set-equal))

(addtest (deoxybyte-utilities-tests) linear-set-equal/2
  (test-list-of-lists (lambda (x y &rest args)
                        (declare (ignore args))
                        (equal x y)) #'linear-set-equal))

(addtest (deoxybyte-utilities-tests) linear-set-equal/3
  (ensure (not (linear-set-equal '(1 2 3) '(2 3)))))
