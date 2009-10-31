;;;
;;; Copyright (C) 2009 Keith James. All rights reserved.
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

(in-package :deoxybyte-utilities)

(defstruct queue
  "A FIFO queue. For consistency, all functions that operate on a
QUEUE carry the QUEUE- prefix, even though this results in tautology
for some functions, such as QUEUE-ENQUEUE/QUEUE-DEQUEUE. This is a
standard way of making a linked-list queue; the QUEUE structure
contains pointers to both the head and tail of a single list."
  (head nil :type list)
  (tail nil :type list))

(defun queue-enqueue (item queue)
  "Inserts ITEM at the end of QUEUE and returns QUEUE."
  (let ((new-tail (list item)))
    (if (queue-empty-p queue)
        (setf (queue-head queue) new-tail
              (queue-tail queue) new-tail)
      (setf (cdr (queue-tail queue)) new-tail
            (queue-tail queue) new-tail)))
  queue)

(defun queue-dequeue (queue)
  "Removes one item from the front of QUEUE and returns two values.
The first value is the item, or NIL if QUEUE was empty and the second
value is T if an item was dequeued, or NIL otherwise."
  (let ((presentp (not (null (queue-head queue))))
        (item (pop (queue-head queue))))
    (when (null (queue-head queue))
      (setf (queue-tail queue) nil))
    (values item presentp)))

(defun queue-dequeue-if (test queue &key key)
  "Removes items from QUEUE that pass predicate TEST, returning two
values, QUEUE and a list of the deleted items. The KEY keyword
argument is the same as for C:DELETE-IF."
  (let ((items (queue-head queue))
        (key (cond ((null key)
                    #'identity)
                   ((functionp key)
                    key)
                   (t
                    (fdefinition key)))))
    (loop
       for item in items
       if (funcall test (funcall key item))
       collect item into fail
       else
       collect item into pass
       finally (progn
                 (setf (queue-head queue) pass
                       (queue-tail queue) (last pass))
                 (return (values queue fail))))))

(defun queue-clear (queue)
  "Removes all items from queue, returning two values, the queue and a
  list of all the items that were in the queue."
  (let ((items (queue-head queue)))
    (setf (queue-head queue) nil
          (queue-tail queue) nil)
    (values queue items)))

(defun queue-first (queue)
  "Returns the first item in QUEUE, without removing it."
  (first (queue-head queue)))

(defun queue-last (queue)
  "Returns the last item in QUEUE, without removing it."
  (first (queue-tail queue)))

(defun queue-nth (n queue)
  "Returns the Nth item in QUEUE."
  (nth n (queue-head queue)))

(defun queue-empty-p (queue)
  "Returns T if QUEUE is empty, or NIL otherwise."
  (null (queue-head queue)))

(defun queue-length (queue)
  "Returns the length of QUEUE."
  (list-length (queue-head queue)))

(defun queue-position (item queue &key key (test #'eql))
  "Returns the zero-based index of ITEM in QUEUE, or NIL if ITEM is
not present. The TEST and KEY keyword arguments are the same as for
CL:POSITION."
  (position item (queue-head queue) :key key :test test))

(defun queue-delete (item queue &key key (test #'eql))
  "Deletes the first occurrence of ITEM in QUEUE, if present,
returning QUEUE. The TEST and KEY keyword arguments are the same as
for CL:DELETE."
  (setf (queue-head queue) (delete item (queue-head queue) :key key :test test)
        (queue-tail queue) (last (queue-head queue)))
  queue)
