;;;
;;; Copyright (C) 2009-2010 Keith James. All rights reserved.
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

(addtest (deoxybyte-utilities-tests) queue/1
  (ensure (queue-p (make-queue))))

(addtest (deoxybyte-utilities-tests) queue-enqueue/1
  (let ((queue (make-queue))
        (items (iota 10)))
    (ensure-same queue (queue-enqueue (first items) queue))
    (dolist (item (rest items))
      (queue-enqueue item queue))
    (ensure (equal items (queue-head queue)))
    (ensure (equal (car (last items)) (queue-last queue)))))

(addtest (deoxybyte-utilities-tests) queue-dequeue/1
  (let ((queue (make-queue))
        (items (iota 10)))
    (dolist (item items)
      (queue-enqueue item queue))
    (dolist (item items)
      (multiple-value-bind (x presentp)
          (queue-dequeue queue)
        (ensure (= item x))
        (ensure presentp)))
    (ensure (queue-empty-p queue))
    (multiple-value-bind (x presentp)
        (queue-dequeue queue)
      (ensure-null x)
      (ensure (not presentp)))))

(addtest (deoxybyte-utilities-tests) queue-dequeue-if/1
  (let ((queue (make-queue))
        (items (iota 10)))
    (dolist (item items)
      (queue-enqueue item queue))
    (multiple-value-bind (q x)
        (queue-dequeue-if #'oddp queue)
      (ensure (equal '(0 2 4 6 8) (queue-head q)))
      (ensure-same queue q)
      (ensure (equal '(1 3 5 7 9) x)))))

(addtest (deoxybyte-utilities-tests) queue-dequeue-if/2
  (let ((queue (make-queue))
        (items (iota 10)))
    (dolist (item items)
      (queue-enqueue (list item) queue))
    (multiple-value-bind (q x)
        (queue-dequeue-if #'oddp queue :key #'first)
      (ensure (equalp '((0) (2) (4) (6) (8)) (queue-head q)))
      (ensure-same queue q)
      (ensure (equalp '((1) (3) (5) (7) (9)) x)))))
  
(addtest (deoxybyte-utilities-tests) queue-clear/1
  (let ((queue (make-queue))
        (items (iota 10)))
    (dolist (item items)
      (queue-enqueue item queue))
    (multiple-value-bind (q x)
        (queue-clear queue)
      (ensure (queue-empty-p q))
      (ensure-same queue q)
      (ensure (equal items x)))))
