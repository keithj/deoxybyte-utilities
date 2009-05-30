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

(in-package :uk.co.deoxybyte-utilities-test)

(addtest (deoxybyte-utilities-tests) iota/1
  (ensure (equal '(0 0 0 0 0) (iota 5 0 0)))
  (ensure (equal '(0 1 2 3 4) (iota 5)))
  (ensure (equal '(10 11 12 13 14) (iota 5 10)))
  (ensure (equal '(0 2 4 6 8) (iota 5 0 2)))
  (ensure (equal '(0 -1 -2 -3 -4) (iota 5 0 -1)))
  (ensure (equal '(0 -2 -4 -6 -8) (iota 5 0 -2))))

(addtest (deoxybyte-utilities-tests) make-number-gen/1
  (let ((gen (make-number-gen)))
    (ensure (equal '(0 1 2 3 4) (loop
                                   repeat 5
                                   collect (next gen)))))
  (let ((gen (make-number-gen 10)))
    (ensure (equal '(10 11 12 13 14) (loop
                                        repeat 5
                                        collect (next gen)))))
  (let ((gen (make-number-gen 0 2)))
    (ensure (equal '(0 2 4 6 8) (loop
                                   repeat 5
                                   collect (next gen))))))

(addtest (deoxybyte-utilities-tests) numeric-selector/1
  ; 5 bins, defaulting to integer range
  (with-numeric-selector (select-bin 5 :out-of-bounds :ignore)
    (let ((bins (make-array 5 :initial-element 0)))
      (loop
         for i from 0 below 10 ; 10 rounds
         do (loop
               for j from -3 upto 7 ; 5 in each bin, 3 outside low & high
               do (let ((b (select-bin j)))
                    (when b
                      (incf (aref bins b))))))
      (ensure (equalp #(10 10 10 10 10) bins)))))

(addtest (deoxybyte-utilities-tests) numeric-selector/2
  (with-numeric-selector (select-bin 5 :out-of-bounds :include)
    (let ((bins (make-array 5 :initial-element 0)))
      (loop
         for i from 0 below 10 ; 10 rounds
         do (loop
               for j from -3 to 7 ; 5 in each bin, 3 outside low & high
               do (let ((b (select-bin j)))
                    (when b
                        (incf (aref bins b))))))
      (ensure (equalp #(40 10 10 10 40) bins))))
  (ensure-condition invalid-argument-error
    (with-numeric-selector (select-bin 5 :out-of-bounds :error)
      (select-bin -1)))
  (ensure-condition invalid-argument-error
    (with-numeric-selector (select-bin 5 :out-of-bounds :error)
      (select-bin 5))))

(addtest (deoxybyte-utilities-tests) categorical-binner/1
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
    (ensure (equalp #(3 3 2 2) bins))
    (ensure (= 1 out))))
