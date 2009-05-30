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

 (let ((vec1 (make-array 10 :initial-contents '(a b c d e a g h i j)))
       (vec2 "abcdeaghij"))
   (addtest (deoxybyte-utilities-tests) vector-positions/1
    (ensure (equal '(0 5) (vector-positions 'a vec1))))

   (addtest (deoxybyte-utilities-tests) vector-positions/2
     (ensure (equal '(5) (vector-positions 'a vec1 :start 1))))

   (addtest (deoxybyte-utilities-tests) vector-positions/3
     (ensure (equal '(0) (vector-positions 'a vec1 :end 2))))

   (addtest (deoxybyte-utilities-tests) vector-positions/4
     (ensure-null (vector-positions 'a vec1 :start 1 :end 4)))

   (addtest (deoxybyte-utilities-tests) vector-positions/5
     (ensure (equal '(0 5) (vector-positions #\a vec2 :test #'char=)))))

(let ((vec1 (make-array 10 :initial-contents '(a b c d e a g h i j)))
      (vec2 "abcdeaghij"))
  (addtest (deoxybyte-utilities-tests) vector-split-indices/1
    (multiple-value-bind (starts ends)
        (vector-split-indices 'a vec1)
      (ensure (equal '(0 1 6) starts))
      (ensure (equal '(0 5 10) ends))))

  (addtest (deoxybyte-utilities-tests) vector-split-indices/2
    (multiple-value-bind (starts ends)
        (vector-split-indices 'a vec1 :start 1)
      (ensure (equal '(1 6) starts))
      (ensure (equal '(5 10) ends))))

  (addtest (deoxybyte-utilities-tests) vector-split-indices/3
    (multiple-value-bind (starts ends)
        (vector-split-indices 'a vec1 :end 2)
      (ensure (equal '(0 1) starts))
      (ensure (equal '(0 2) ends))))

  (addtest (deoxybyte-utilities-tests) vector-split-indices/4
    (multiple-value-bind (starts ends)
        (vector-split-indices 'a vec1 :start 1 :end 4)
      (ensure-null starts)
      (ensure-null ends)))

  (addtest (deoxybyte-utilities-tests) vector-split-indices/5
    (multiple-value-bind (starts ends)
        (vector-split-indices #\a vec2 :test #'char=)
      (ensure (equal '(0 1 6) starts))
      (ensure (equal '(0 5 10) ends)))))


 (let ((vec1 (make-array 10 :initial-contents '(a b c d e a g h i j)))
       (vec2 "abcdeaghij"))
   (addtest (deoxybyte-utilities-tests) vector-split/1
     (ensure (equalp '(#() #(b c d e) #(g h i j))
                     (vector-split 'a vec1))))

   (addtest (deoxybyte-utilities-tests) vector-split/2
     (ensure (equalp '(#(b c d e) #(g h i j))
                     (vector-split 'a vec1 :remove-empty-subseqs t))))

   (addtest (deoxybyte-utilities-tests) vector-split/3
     (ensure (equalp '(#(b c d e) #(g h i j))
                     (vector-split 'a vec1 :start 1))))

   (addtest (deoxybyte-utilities-tests) vector-split/4
     (ensure (equalp '(#() #(b))
                     (vector-split 'a vec1 :end 2))))

   (addtest (deoxybyte-utilities-tests) vector-split/5
     (ensure (equalp '(#(b c d))
                     (vector-split 'a vec1 :start 1 :end 4))))

   (addtest (deoxybyte-utilities-tests) vector-split/6
     (ensure (equal '("" "bcde" "ghij")
                    (vector-split #\a vec2 :test #'char=)))))

(addtest (deoxybyte-utilities-tests) vector-errors/1
  (let ((vec1 (make-array 10 :initial-contents '(a b c d e a g h i j))))
    (dolist (fn (list #'vector-positions #'vector-split
                      #'vector-split-indices))
      (ensure-error
        (funcall fn 'a vec1 :start -1))
      (ensure-error
        (funcall fn 'a vec1 :end -1))
      (ensure-condition invalid-argument-error
        (funcall fn 'a vec1 :end 99))
      (ensure-condition invalid-argument-error
        (funcall fn 'a vec1 :start 1 :end 0)))))
