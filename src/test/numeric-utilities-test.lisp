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

(in-package :cl-gp-utilities-test)

(in-suite cl-gp-utilities-system:testsuite)

(test iota
  (is (equal '(0 0 0 0 0) (iota 5 0 0)))
  (is (equal '(0 1 2 3 4) (iota 5)))
  (is (equal '(10 11 12 13 14) (iota 5 10)))
  (is (equal '(0 2 4 6 8) (iota 5 0 2)))
  (is (equal '(0 -1 -2 -3 -4) (iota 5 0 -1)))
  (is (equal '(0 -2 -4 -6 -8) (iota 5 0 -2))))

(test make-number-gen
  (let ((gen (make-number-gen)))
    (is (equal '(0 1 2 3 4) (loop
                               repeat 5
                               collect (next gen)))))
  (let ((gen (make-number-gen 10)))
    (is (equal '(10 11 12 13 14) (loop
                                    repeat 5
                                    collect (next gen)))))
  (let ((gen (make-number-gen 0 2)))
    (is (equal '(0 2 4 6 8) (loop
                               repeat 5
                               collect (next gen))))))

(test numeric-binner
  (let ((fn (define-numeric-binner 5 5))
        (bins nil)
        (low nil)
        (high))
    (loop
       for i from 0 below 10 ; 10 rounds
       do (loop
             for j from 0 to 7 ; 5 in each bin, 3 outside high
             do (multiple-value-setq (bins low high)
                  (funcall fn j))))
    (is (equalp #(10 10 10 10 10) bins))
    (is (= 0 low))
    (is (= 30 high))))

(test categorical-binner
  (let ((fn (define-categorical-binner x
              (and (oddp x) (plusp x))
              (and (oddp x) (minusp x))
              (and (evenp x) (plusp x))
              (and (evenp x) (minusp x))))
        (bins nil)
        (out nil))
    (dolist (i '(-5 -4 -3 -2 -1 0 1 2 3 4 5))
      (multiple-value-setq (bins out)
        (funcall fn i)))
    (is (equalp #(3 3 2 2) bins))
    (is (= 1 out))))

