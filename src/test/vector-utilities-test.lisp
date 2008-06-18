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

(test vector-positions
  (let ((vec1 (make-array 10 :initial-contents '(a b c d e a g h i j)))
        (vec2 "abcdeaghij"))
    (is (equal '(0 5) (vector-positions 'a vec1)))
    (is (equal '(5) (vector-positions 'a vec1 :start 1)))
    (is (equal '(0) (vector-positions 'a vec1 :end 2)))
    (is (null (vector-positions 'a vec1 :start 1 :end 4)))
    (is (equal '(0 5) (vector-positions #\a vec2 :test #'char=)))))

(test vector-split-indices
  (let ((vec1 (make-array 10 :initial-contents '(a b c d e a g h i j)))
        (vec2 "abcdeaghij"))
    (multiple-value-bind (starts ends)
        (vector-split-indices 'a vec1)
      (is (equal '(0 1 6) starts))
      (is (equal '(0 5 10) ends)))
    (multiple-value-bind (starts ends)
        (vector-split-indices 'a vec1 :start 1)
      (is (equal '(1 6) starts))
      (is (equal '(5 10) ends)))
    (multiple-value-bind (starts ends)
        (vector-split-indices 'a vec1 :end 2)
      (is (equal '(0 1) starts))
      (is (equal '(0 2) ends)))
    (multiple-value-bind (starts ends)
        (vector-split-indices 'a vec1 :start 1 :end 4)
      (is (null starts))
      (is (null ends)))
    (multiple-value-bind (starts ends)
        (vector-split-indices #\a vec2 :test #'char=)
      (is (equal '(0 1 6) starts))
      (is (equal '(0 5 10) ends)))))

(test vector-split
  (let ((vec1 (make-array 10 :initial-contents '(a b c d e a g h i j)))
        (vec2 "abcdeaghij"))
    (is (equalp '(#() #(b c d e) #(g h i j))
                (vector-split 'a vec1)))
    (is (equalp '(#(b c d e) #(g h i j))
                (vector-split 'a vec1 :remove-empty-subseqs t)))
    (is (equalp '(#(b c d e) #(g h i j))
                (vector-split 'a vec1 :start 1)))
    (is (equalp '(#() #(b))
                (vector-split 'a vec1 :end 2)))
    (is (equalp '(#(b c d))
                (vector-split 'a vec1 :start 1 :end 4)))
    (is (equal '("" "bcde" "ghij")
               (vector-split #\a vec2 :test #'char=)))))
