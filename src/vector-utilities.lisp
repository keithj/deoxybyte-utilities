;;;
;;; Copyright (C) 2008-2010 Keith James. All rights reserved.
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

(defun vector-positions (elt vector &key (start 0) end (test #'eql))
  "Returns a list of indices into VECTOR between START and END where
ELT is present according to TEST (which defaults to EQL)."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type vector vector)
           (type function test))
  (let ((end (or end (length vector))))
    (declare (type vector-index start end))
    (check-arguments (<= 0 start end (length vector)) (start end)
                     "start must be >= 0 and be <= end")
    (loop for i from start below end
       when (funcall test elt (aref vector i))
       collect i)))

(defun vector-split-indices (elt vector &key (start 0) end (test #'eql))
  "Returns two values, a list of start indices and a list of end
indices into VECTOR between START and END such that if used as
start/end arguments to subseq, VECTOR will be split on ELT. ELT is
compared with elements in VECTOR using TEST, which defaults to EQL."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type vector vector))
  (let ((end (or end (length vector))))
    (declare (type vector-index start end))
    (check-arguments (<= 0 start end (length vector)) (start end)
                     "start must be >= 0 and be <= end")
    (let ((positions (vector-positions elt vector
                                       :start start :end end :test test)))
      (if positions
          (loop
             with starts = ()
             with ends = ()
             for pos of-type vector-index in positions
             and prev = start then (the vector-index (1+ pos))
             maximize pos into last-pos
             do (progn
                  (push prev starts)
                  (push pos ends))
             finally (progn
                       (push (the vector-index (1+ last-pos)) starts)
                       (push end ends)
                       (return
                         (values (nreverse starts) (nreverse ends)))))
        nil))))

(defun vector-split (elt vector &key (start 0) end (test #'eql)
                     remove-empty-subseqs displace-to-vector)
  "Returns a list of vectors made by splitting VECTOR at ELT, between
START and END. ELT is compared with elements in VECTOR using TEST,
which defaults to EQL. If REMOVE-EMPTY-SUBSEQS is T, any empty
subsequences will be omitted from the returned list. If
DISPLACE-TO-VECTOR id T, the returned subsequences will be displaced
to the actual subsequences within VECTOR and will therefore share
structure with VECTOR."
  (let ((end (or end (length vector)))
        (elt-type (array-element-type vector)))
    (check-arguments (<= 0 start end (length vector)) (start end)
                     "start must be >= 0 and be <= end")
    (multiple-value-bind (starts ends)
        (vector-split-indices elt vector :start start :end end :test test)
      (cond ((and starts ends)
             (loop
                for i in starts
                for j in ends  
                when (not (and remove-empty-subseqs
                               (= i j)))
                collect (if displace-to-vector
                             (make-array (- j i)
                                         :element-type elt-type
                                         :displaced-to vector
                                         :displaced-index-offset i)
                          (subseq vector i j))))
            (displace-to-vector
             (make-array (- end start)
                         :element-type elt-type
                         :displaced-to vector
                         :displaced-index-offset start))
            (t
             (list (subseq vector start end)))))))

(defun binary-search (vector item &key (test #'<) key
                      (start 0) (end (length vector)))
  "Searches for ITEM in VECTOR by binary search.

Arguments:

- vector (vector): A sorted vector to be searched.
- item (object): The item to be found.

Key:

- test (function): The test used to compare ITEM with VECTOR elements.
- key (function): A key function with which to transform VECTOR elements.
- start (fixnum): The lower bound index of the search in VECTOR.
- end (fixnum): The upper bound index of the search in VECTOR.

Returns:
- An object, or NIL."
  (labels ((bin-search (start end)
             (when (< start end)
               (let ((mid (+ start (floor (- end start) 2))))
                 (let ((mid-item (if key
                                     (funcall key (aref vector mid))
                                   (aref vector mid))))
                   (cond ((funcall test item mid-item)
                          (bin-search start mid))
                         ((funcall test mid-item item)
                          (bin-search (1+ mid) end))
                         (t
                          (aref vector mid))))))))
      (bin-search start end)))
