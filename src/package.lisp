
(defpackage #:cl-gp-utilities
  (:use #:common-lisp)
  (:nicknames #:gpu)
  (:documentation "General purpose utilities.")
  (:export
   ;; Constants
   #:+whitespace-chars+
   #:+whitespace-codes+
   ;; Conditions

   ;; Classes

   ;; Generics

   ;; Functions
   #:vector-positions
   #:vector-split-indices
   #:vector-split
   #:whitespace-byte-p
   #:whitespace-bytes-p
   #:has-byte-at-p
   #:starts-with-byte-p
   #:make-sb-string
   #:concat-into-sb-string
   #:control-char-p
   #:whitespace-char-p
   #:whitespace-string-p
   #:has-char-at-p
   #:starts-with-char-p
   #:every-char-p
   #:concat-strings
   #:write-wrapped-string
   #:all-superclasses
   #:all-specialized-methods
   #:all-specialized-generic-functions
   #:all-external-generic-functions
   ;; Macros
   #:copy-array
   #:assocdr
   #:rassocar
   ;;Types
   #:array-index))
