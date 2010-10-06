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

(in-package :cl-user)

(defpackage :uk.co.deoxybyte-utilities
  (:use #:common-lisp)
  (:nicknames
   #:deoxybyte-utilities
   #:dxu)
  (:export
   ;; Conditions
   #:check-arguments
   #:simple-text-condition
   #:formatted-condition
   #:invalid-argument-error
   #:missing-argument-error
   #:invalid-operation-error
   #:deprecation-warning
   #:message-of

   ;; Deoxybyte-utilities
   #:with-gensyms
   #:funcall-if-fn
   #:copy-vector
   #:defgenerator
   #:current
   #:next
   #:has-more-p
   #:consume
   #:collect
   #:discard
   #:discarding-if

   ;; Type utilities
   #:octet
   #:nibble
   #:uint8
   #:int8
   #:uint16
   #:int16
   #:uint32
   #:int32
   #:array-index
   #:vector-index
   #:simple-octet-vector

   ;; CLOS utilities
   #:has-superclass-p
   #:direct-superclasses
   #:direct-subclasses
   #:all-superclasses
   #:all-classes
   #:all-external-classes
   #:all-specialized-methods
   #:all-specialized-generic-functions
   #:all-external-generic-functions
   #:all-slots
   #:slot-documentation

   ;; Cons utilities
   #:assocdr
   #:rassocar
   #:rplassoc
   #:assocpush
   #:assocpop
   #:dotted-pair-p
   #:proper-list-p
   #:splice
   #:nsplice
   #:intersperse
   #:flatten
   #:exactly-n
   #:exactly-one
   #:collect-key-values
   #:key-value
   #:remove-key-values
   #:modify-key-value

   ;; Vector utilities
   #:vector-positions
   #:vector-split-indices
   #:vector-split
   #:binary-search

   ;; Set utilities
   #:linear-union
   #:linear-intersection
   #:linear-set-difference
   #:linear-set-exclusive-or
   #:linear-subsetp
   #:linear-set-equal

   ;; String utilities
   #:control-char-p
   #:whitespace-char-p
   #:whitespace-string-p
   #:content-string-p
   #:empty-string-p
   #:contains-char-p
   #:has-char-at-p
   #:starts-with-char-p
   #:ends-with-char-p
   #:every-char-p
   #:starts-with-string-p
   #:ends-with-string-p
   #:concat-strings
   #:txt
   #:str
   #:string-positions
   #:string-split-indices
   #:string-split

   ;; Octet vector utilities
   #:octets-to-string
   #:string-to-octets

   ;; Numeric utilities
   #:iota
   #:number-generator
   #:with-numeric-selector
   #:define-categorical-binner

   ;; Finite state machine
   #:defsmfun
   #:defsm

   ;; Queue
   #:queue
   #:queue-p
   #:queue-head
   #:queue-first
   #:queue-last
   #:make-queue
   #:queue-enqueue
   #:queue-dequeue
   #:queue-dequeue-if
   #:queue-clear
   #:queue-first
   #:queue-last
   #:queue-nth
   #:queue-empty-p
   #:queue-delete)
  (:documentation "The deoxybyte-utilities system provides general
purpose utilities. These are for the most part simple, standalone
functions and macros. Once a particular function or macro has been
written more than once or twice in other systems, it normally gets
moved here.

Given the fluid nature of the contents of this system, its package's
exported symbols are intended primarily for use by other deoxybyte
packages."))
