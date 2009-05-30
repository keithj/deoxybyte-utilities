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

(in-package :uk.co.deoxybyte-utilities)

(defmacro defsmfun (name args bindings (&rest states))
  "Returns a function that encapsulates a finite state machine over STATES.

Arguments:

- name (symbol): The name of the function.
- args (lambda list): The function lambda list.
- bindings (list): A list of parallel bindings (as for LET).

Rest:

- states (list): A list of forms describing the state
  transitions. Each state form consists of a symbol that denotes a
  current state and a series of conditional forms that act indicate
  potential states that may be reached from the current state. The
  tests are evaluated in turn as in a COND form.

     (current-state (test1 next-state1 &rest actions)
                    (test2 next-state2 &rest actions)
                    (t nil))

  When a test returns T, the machine moves to the next state once the
  current state's action forms are evaluated. A next state of NIL
  indicates termination, at which point the function returns the
  result of evaluating the last action form.

  For example, the following definition returns a function that
  partitions a DNA sequence into base triples (codons).

   ;;; (defsmfun codon-iterator (seq &key (start 0))
   ;;;   ((i start)
   ;;;    (len (length-of seq))
   ;;;    (codons ())
   ;;;    (codon ()))
   ;;;   ((base0 ((< i len) base1
   ;;;            (setf codon (list (element-of seq i)))
   ;;;            (incf i))
   ;;;            (t nil
   ;;;               (reverse codons)))
   ;;;    (base1 ((< i len) base2
   ;;;            (push (element-of seq i) codon)
   ;;;            (incf i))
   ;;;           (t nil
   ;;;              (reverse codons)))
   ;;;    (base2 ((< i len) base0
   ;;;            (push (element-of seq i) codon)
   ;;;            (push (reverse codon) codons)
   ;;;            (incf i))
   ;;;           (t nil
   ;;;              (reverse codons)))))

Returns:

- A function."
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
  "Defines a finite state machine over STATES.

Arguments:

- states (list): A list of forms describing the state transitions. See
  {defmacro defsmfun} .

For example:

   ;;; (let* ((seq (make-dna \"atggtgcct\"))
   ;;;        (i 0)
   ;;;        (len (length-of seq))
   ;;;        (codons ())
   ;;;        (codon ()))
   ;;;   (defsm ((base0 ((< i len) base1
   ;;;                   (setf codon (list (element-of seq i)))
   ;;;                   (incf i))
   ;;;                  (t nil
   ;;;                     (reverse codons)))
   ;;;           (base1 ((< i len) base2
   ;;;                   (push (element-of seq i) codon)
   ;;;                   (incf i))
   ;;;                  (t nil
   ;;;                     (reverse codons)))
   ;;;           (base2 ((< i len) base0
   ;;;                   (push (element-of seq i) codon)
   ;;;                   (push (reverse codon) codons)
   ;;;                   (incf i))
   ;;;                  (t nil
   ;;;                     (reverse codons)))))"
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
                             (t ; this may be redundant if the user
                                ; supplies their own catch-all clause
                              '((return)))))))
         acceptors)
      (t
       (return)))))
