;;;
;;; Copyright (c) 2008-2012 Keith James. All rights reserved.
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

(addtest (deoxybyte-utilities-tests) assocdr/1
  (let ((keys '(a b c))
        (vals '(1 2 3)))
    (mapc #'(lambda (key val)
              (ensure (= val (assocdr key (pairlis keys vals)))))
          keys vals)))

(addtest (deoxybyte-utilities-tests) assocdr/2
  (let ((keys '("a" "b" "c"))
        (vals '(1 2 3)))
    (mapc #'(lambda (key val)
              (ensure (= val (assocdr key (pairlis keys vals)
                                      :test #'string=))))
          keys vals)))

(addtest (deoxybyte-utilities-tests) rassocar/1
  (let ((keys '(a b c))
        (vals '(1 2 3)))
    (mapc #'(lambda (key val)
              (ensure (eql key (rassocar val (pairlis keys vals)))))
          keys vals)))

(addtest (deoxybyte-utilities-tests) rassocar/2
  (let ((keys '(a b c))
        (vals '("1" "2" "3")))
    (mapc #'(lambda (key val)
              (ensure (eql key (rassocar val (pairlis keys vals)
                                         :test #'string=))))
          keys vals)))

(addtest (deoxybyte-utilities-tests) rplassoc/1
  (let ((keys '(a b c))
        (vals '(1 2 3)))
    (let ((alist (pairlis keys vals)))
      (rplassoc 'a alist 99)
      (ensure (= 99 (assocdr 'a alist))))))

(addtest (deoxybyte-utilities-tests) rplassoc/2
  (let ((keys '("a" "b" "c"))
        (vals '(1 2 3)))
    (let ((alist (pairlis keys vals)))
      (rplassoc "a" alist 99
                :test #'string=)
      (ensure (= 99 (assocdr "a" alist
                             :test #'string=))))))

(addtest (deoxybyte-utilities-tests) assocpush/1
  (let ((keys '(a b))
        (vals '((1 2) (3 4))))
    (let ((alist (pairlis keys vals)))
      (assocpush 'a alist 99)
      (ensure (equal '(99 1 2) (assocdr 'a alist))))))

(addtest (deoxybyte-utilities-tests) assocpush/2
  (let ((keys '("a" "b"))
        (vals '((1 2) (3 4))))
    (let ((alist (pairlis keys vals)))
      (assocpush "a" alist 99
                 :test #'string=)
      (ensure (equal '(99 1 2) (assocdr "a" alist
                                        :test #'string=))))))

(addtest (deoxybyte-utilities-tests) assocpop/1
  (let ((keys '(a b))
        (vals '((1 2) (3 4))))
    (let ((alist (pairlis keys vals)))
      (ensure (= 1 (assocpop 'a alist)))
      (ensure (equal '(2) (assocdr 'a alist))))))

(addtest (deoxybyte-utilities-tests) assocpop/2
  (let ((keys '("a" "b"))
        (vals '((1 2) (3 4))))
    (let ((alist (pairlis keys vals)))
      (ensure (= 1 (assocpop "a" alist
                             :test #'string=)))
      (ensure (equal '(2) (assocdr "a" alist
                                   :test #'string=))))))

(addtest (deoxybyte-utilities-tests) splice/1
  (let ((x (list 1 2 3))
        (y 99)) ; atom
    (ensure (equal '(99 1 2 3) (splice x y 0)))
    (ensure (equal '(1 99 2 3) (splice x y 1)))
    (ensure (equal '(1 2 99 3) (splice x y 2)))
    (ensure (equal '(1 2 3 99) (splice x y 3)))))

(addtest (deoxybyte-utilities-tests) splice/2
  (let ((x (list 1 2 3))
        (y (list 99 77))) ; list
    (ensure (equal '(99 77 1 2 3) (splice x y 0)))
    (ensure (equal '(1 99 77 2 3) (splice x y 1)))
    (ensure (equal '(1 2 99 77 3) (splice x y 2)))
    (ensure (equal '(1 2 3 99 77) (splice x y 3)))))

(addtest (deoxybyte-utilities-tests) intersperse/1
  (ensure (equal '(a x b x c) (intersperse '(a b c) 'x)))
  (ensure (equal '(a x b) (intersperse '(a b) 'x)))
  (ensure (equal '(a) (intersperse '(a) 'x)))
  (ensure (equal '() (intersperse '() 'x))))

(addtest (deoxybyte-utilities-tests) collect-key-values/1
  (let ((arg-list '(:a 1 :b 2 :c 3)))
    (multiple-value-bind (args vals)
        (collect-key-values '(:a :b) arg-list)
      (ensure (equal '(:a :b) args))
      (ensure (equal '( 1 2) vals)))))

(addtest (deoxybyte-utilities-tests) key-value/1
  (let ((arg-list '(:a 1 :b 2 :c 3)))
    (ensure (= 1 (key-value :a arg-list)))
    (ensure (= 2 (key-value :b arg-list)))
    (ensure (= 3 (key-value :c arg-list)))))

(addtest (deoxybyte-utilities-tests) remove-key-values/1
  (let ((arg-list '(:a 1 :b 2 :c 3)))
    (multiple-value-bind (retained removed)
        (remove-key-values '(:a :c) arg-list)
      (ensure (equal '(:b 2) retained))
      (ensure (= 1 (assocdr :a removed)))
      (ensure (= 3 (assocdr :c removed))))))

(addtest (deoxybyte-utilities-tests) modify-key-value/1
  (let ((arg-list '(:a 1 :b 2 :c 3))
        (fn (lambda (x &optional (y 0))
              (+ 10 x y))))
    (ensure (equal '(:a 11 :b 2 :c 3)
                   (modify-key-value :a arg-list fn)))
    (ensure (equal '(:a 1 :b 12 :c 3)
                   (modify-key-value :b arg-list fn)))
    (ensure (equal '(:a 1 :b 2 :c 13)
                   (modify-key-value :c arg-list fn)))
    (ensure (equal '(:a 1 :b 2 :c 113)
                   (modify-key-value :c arg-list fn 100)))))
