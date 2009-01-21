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

(in-package :cl-gp-utilities)

(defmacro defsmfun (name args bindings (&rest states))
  `(defun ,name ,args
    (let (,@bindings)
      (block nil
        (tagbody
           ,@(apply #'append
                    (mapcar
                     (lambda (state)
                       (destructuring-bind (name &rest acceptors)
                           state
                         (cons name
                               (if (zerop (length acceptors))
                                   '((return))
                                 (make-conditional acceptors)))))
                     states)))))))

(defmacro defsm ((&rest states))
  `(block nil
    (tagbody
       ,@(apply #'append
                (mapcar
                 (lambda (state)
                   (destructuring-bind (name &rest acceptors)
                       state
                     (cons name
                           (if (zerop (length acceptors))
                               '((return))
                             (make-conditional acceptors)))))
                 states)))))

(defun make-conditional (acceptors)
  `((cond
      ,@(mapcar
         (lambda (acceptor)
           (destructuring-bind (test state &rest forms)
               acceptor
             `(,test ,@(cond (state
                              `(,@forms
                                (go ,state)))
                             (forms
                              `((let ((x (progn
                                           ,@forms)))
                                  (return x))))
                             (t
                              '((return)))))))
         acceptors)
      (t
       (return)))))

;; (defsmfun codon-iterator (seq &key (start 0))
;;   ((i start)
;;    (len (length-of seq))
;;    (codons ())
;;    (codon ()))
;;   ((base0 ((< i len) base1
;;            (setf codon (list (element-of seq i)))
;;            (incf i))
;;            (t nil
;;               (reverse codons)))
;;    (base1 ((< i len) base2
;;            (push (element-of seq i) codon)
;;            (incf i))
;;           (t nil))
;;    (base2 ((< i len) base0
;;            (push (element-of seq i) codon)
;;            (push (reverse codon) codons)
;;            (incf i))
;;           (t nil))))

;; (let* ((seq (make-dna "atggtgcct"))
;;        (i 0)
;;        (len (length-of seq))
;;        (codons ())
;;        (codon ()))
;;   (defsm ((base0 ((< i len) base1
;;                   (setf codon (list (element-of seq i)))
;;                   (incf i))
;;                  (t nil
;;                     (reverse codons)))
;;           (base1 ((< i len) base2
;;                   (push (element-of seq i) codon)
;;                   (incf i))
;;                  (t nil))
;;           (base2 ((< i len) base0
;;                   (push (element-of seq i) codon)
;;                   (push (reverse codon) codons)
;;                   (incf i))
;;                  (t nil)))))

;; (define-fsm zombat (x)
;;   ()
;;   ((start ((eql 'foo (first x)) lambard
;;            (format t "Foo!~%"))
;;            ((eql 'bar (first x)) lard
;;             (format t "Bar!~%"))
;;            (t flub))
;;    (lambard ((eql 'foo (second x)) pling
;;              (format t "Moo!~%"))
;;             ((eql 'baz (second x)) lard
;;              (format t "Lardy!~%"))
;;             (t flub))
;;    (lard ((= 2 (length x)) flub)) 
;;    (zangbot)
;;    (pling (t nil
;;              (format t "Nice!~%")))
;;    (flub (t nil
;;             (format t "flub~%")))))
