;;;
;;; Copyright (C) 2007-2008, Keith James. All rights reserved.
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

(defpackage #:cl-gp-utilities
  (:use #:common-lisp)
  (:nicknames #:gpu)
  (:documentation "General purpose utilities.")
  (:export
   ;; Constants

   ;; Conditions
   #:invalid-argument-error
   #:invalid-operation-error

   ;; Classes

   ;; Generics

   ;; Functions
   #:collect-args
   #:arg-value
   #:remove-args
   #:modify-arg
   #:splice
   #:nsplice
   #:interleave
   #:vector-positions
   #:vector-split-indices
   #:vector-split
   #:string-positions
   #:string-split-indices
   #:string-split
   #:whitespace-byte-p
   #:whitespace-bytes-p
   #:content-bytes-p
   #:has-byte-at-p
   #:starts-with-byte-p
   #:make-sb-string
   #:concat-into-sb-string
   #:control-char-p
   #:whitespace-char-p
   #:whitespace-string-p
   #:content-string-p
   #:empty-string-p
   #:contains-char-p
   #:has-char-at-p
   #:starts-with-char-p
   #:every-char-p
   #:concat-strings
   #:msg

   #:iota
   #:define-numeric-binner
   #:define-categorical-binner
   #:current
   #:next
   #:has-more-p
   #:consume
   #:collect
   #:make-number-gen

   #:all-superclasses
   #:all-specialized-methods
   #:all-specialized-generic-functions
   #:all-external-generic-functions

   ;; Macros
   #:with-gensyms
   #:copy-array
   #:assocdr
   #:rassocar
   #:rplassoc
   #:assocpush
   #:assocpop
   #:assocpush+
   #:defsmfun
   #:defsm

   #:with-numeric-selector

   ;;Types
   #:array-index))
