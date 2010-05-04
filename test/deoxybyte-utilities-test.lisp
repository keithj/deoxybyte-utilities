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

(in-package :uk.co.deoxybyte-utilities-test)

(deftestsuite deoxybyte-utilities-tests ()
  ())

(defclass x1 ()
  ((slotx1 :initarg :slotx1
           :reader slotx1-of)))

(defclass y1 ()
  ((sloty1 :initarg :sloty1
           :reader sloty1-of)))

(defclass x2 (x1)
  ((slotx2 :initarg :slotx2
           :reader slotx2-of)))

(defclass x3 (x2 y1)
  ((slotx3 :initarg :slotx3
           :reader slotx3-of)))

(defun find-slot-reader-method (class-name generic-function)
  (find-method generic-function '() (list (find-class class-name))))

(defun seq-iterator (seq)
  (let ((n (length seq))
        (i 0))
    (defgenerator
        (more (< i n))
        (next (prog1
                  (elt seq i)
                (incf i))))))

(addtest (deoxybyte-utilities-tests) funcall-if-fn/1
  (ensure (equal "ABCDEF" (funcall-if-fn nil "ABCDEF")))
  (ensure (equal "abcdef" (funcall-if-fn #'string-downcase "ABCDEF"))))

(addtest (deoxybyte-utilities-tests) defgenerator/1
  (let ((gen (seq-iterator (iota 10))))
    (ensure (equal (iota 10) (loop
                             while (has-more-p gen)
                             collect (next gen))))
    (ensure (not (has-more-p gen)))))

(addtest (deoxybyte-utilities-tests) collect/1
  (let ((gen (seq-iterator (iota 10))))
    (ensure (equal (iota 1) (collect gen)))
    (ensure (equal (iota 5 1) (collect gen 5)))))

(addtest (deoxybyte-utilities-tests) discard/1
  (let ((gen (seq-iterator (iota 10))))
    (ensure (= 5 (discard gen 5)))
    (ensure (= 5 (discard gen 10)))))

(addtest (deoxybyte-utilities-tests) discarding-if/1
  (let ((gen (discarding-if #'evenp (seq-iterator (iota 1)))))
    (ensure (equal '() (loop
                           while (has-more-p gen)
                           collect (next gen)))))
  (let ((gen (discarding-if #'oddp (seq-iterator (iota 1)))))
    (ensure (equal '(0) (loop
                           while (has-more-p gen)
                           collect (next gen))))))

(addtest (deoxybyte-utilities-tests) discarding-if/2
  (let ((gen (discarding-if #'null (seq-iterator (iota 10)))))
    (ensure (equal (iota 10) (loop
                              while (has-more-p gen)
                              collect (next gen)))))
  (let ((gen (discarding-if #'oddp (seq-iterator (iota 10)))))
    (ensure (equal '(0 2 4 6 8) (loop
                                   while (has-more-p gen)
                                   collect (next gen)))))
  (let ((gen (discarding-if #'evenp (seq-iterator (iota 10)))))
    (ensure (equal '(1 3 5 7 9) (loop
                                   while (has-more-p gen)
                                   collect (next gen))))))
