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

(in-package :uk.co.deoxybyte-utilities)

;;; TODO - maybe implement these functions with bit-vectors for some
;;; cases

(defun linear-union (list1 list2 &key key test)
  "Returns a list that is the union of LIST1 and LIST2, in linear
time. See CL:UNION for the meaning of KEY and TEST."
  (let ((hash (make-hash-table :size (+ (length list1) (length list2))
                               :test (or test #'eql))))
    (dolist (elt1 list1)
      (setf (gethash (funcall-if-fn key elt1) hash) t))
    (dolist (elt2 list2)
      (let ((elt (funcall-if-fn key elt2)))
        (unless (gethash elt hash)
          (setf (gethash elt hash) t))))
    (loop
       for elt being the hash-keys of hash
       collect elt)))

(defun linear-intersection (list1 list2 &key key test)
  "Returns a list that is the intersection of LIST1 and LIST2, in
linear time. See CL:INTERSECTION for the meaning of KEY and TEST."
  (cond ((null list1)
         nil)
        ((null list2)
         nil)
        (t
         (let* ((len1 (length list1))
                (len2 (length list2))
                (hash (make-hash-table :size (min len1 len2)
                                       :test (or test #'eql))))
           (multiple-value-bind (smaller larger)
               (if (< len1 len2)
                   (values list1 list2)
                 (values list2 list1))
             (dolist (elt smaller)
               (setf (gethash (funcall-if-fn key elt) hash) t))
             (loop
                for elt in larger
                when (gethash (funcall-if-fn key elt) hash)
                collect elt))))))

(defun linear-set-difference (list1 list2 &key key test)
  "Returns a list that is the set difference of LIST1 and LIST2, in
linear time. See CL:SET-DIFFERENCE for the meaning of KEY and TEST."
  (cond ((null list2)
         list1)
        (t
         (let ((hash (make-hash-table :size (length list1)
                                      :test (or test #'eql))))
           (dolist (elt1 list1)
             (setf (gethash (funcall-if-fn key elt1) hash) t))
           (dolist (elt2 list2)
             (remhash (funcall-if-fn key elt2) hash))
           (loop
              for elt being the hash-keys of hash
              collect elt)))))

(defun linear-set-exclusive-or (list1 list2 &key key test)
  "Returns a list that is the set exclusive-or of LIST1 and LIST2, in
linear time. See CL:SET-EXCLUSIVE-OR for the meaning of KEY and TEST."
  (cond ((and (null list1) (null list2))
         nil)
        ((null list1)
         list2)
        ((null list2)
         list1)
        (t
         (linear-set-difference
          (linear-union list1 list2 :key key :test test)
          (linear-intersection list1 list2 :key key :test test)))))

(defun linear-subsetp (list1 list2 &key key test)
  "Returns T if LIST1 is a subset of LIST2, in linear time. See
CL:SUBSETP for the meaning of KEY and TEST."
  (cond ((null list1)
         t)
        (t
         (let* ((len1 (length list1))
                (len2 (length list2))
                (hash (make-hash-table :size (max len1 len2)
                                       :test (or test #'eql))))
           (multiple-value-bind (smaller larger)
               (if (< len1 len2)
                   (values list1 list2)
                 (values list2 list1))
             (dolist (elt larger)
               (setf (gethash (funcall-if-fn key elt) hash) t))
             (loop
                for elt in smaller
                always (gethash (funcall-if-fn key elt) hash)))))))

(defun linear-set-equal (list1 list2  &key key test)
  (and (not (linear-set-difference list1 list2 :key key :test test))
       (not (linear-set-difference list2 list1 :key key :test test))))
