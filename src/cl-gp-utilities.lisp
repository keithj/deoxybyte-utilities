
(in-package :cl-gp-utilities)

(deftype array-subscript ()
  '(and fixnum (integer 0 *)))

;; Array copying macro

(defmacro copy-array (source source-start source-end
                      dest dest-start &optional key)
  "Copies elements SOURCE indices SOURCE-START and SOURCE-END to DEST,
inserting them into DEST at DEST-START onwards. If the function KEY is
supplied, it is applied to each element of SOURCE prior to its
insertion into DEST."
  `(loop for si of-type array-subscript from ,source-start to ,source-end
      for di of-type array-subscript = ,dest-start
      then (the array-subscript (1+ di))
      do (setf (aref ,dest di) ,(if key
                                    `(funcall ,key (aref ,source si))
                                  `(aref ,source si)))))


;; Introspection utility functions

(defun all-superclasses (class &optional
                         (ceiling (find-class 'standard-object)))
  "Returns a list of all superclasses of CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  #-(or :sbcl :cmu :lispworks)
  (error "ALL-SUPERCLASSES not supported on this implementation.")
  #+:sbcl (set-difference (sb-mop:compute-class-precedence-list class)
                          (sb-mop:compute-class-precedence-list ceiling))
  #+:cmu (set-difference (mop:compute-class-precedence-list class)
                         (mop:compute-class-precedence-list ceiling))
  #+:lispworks (set-difference (clos:class-precedence-list class)
                               (clos:class-precedence-list ceiling)))

(defun all-specialized-methods (class &optional
                                (ceiling (find-class 'standard-object)))
   "Returns a list of all methods specialized on CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
   #-(or :sbcl :cmu :lispworks)
   (error "ALL-SPECIALIZED-METHODS not supported on this
implementation.")
   (apply #'append
         (mapcar #+:sbcl #'sb-mop:specializer-direct-methods
                 #+:cmu #'mop:specializer-direct-methods
                 #+:lispworks #'clos:specializer-direct-methods
                 (all-superclasses class ceiling))))

(defun all-specialized-generic-functions (class &optional
                                          (ceiling (find-class
                                                    'standard-object)))
"Returns a list of all generic functions specialized on CLASS, up to,
but not including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  #-(or :sbcl :cmu :lispworks)
  (error "ALL-SPECIALIZED-GENERIC-FUNCTIONS not supported on this
implementation.")
  (mapcar #+:sbcl #'sb-mop:method-generic-function
          #+:cmu #'mop:method-generic-function
          #+:lispworks #'clos:method-generic-function
          (all-specialized-methods class ceiling)))


;; Lazy initialization proxy

;; The lazy proxy class and reify method were posted by Ron Garret in
;; comp.lang.lisp, 27 Apr 2005. Ron's implementation was not portable
;; (and does not compile on Lispworks) and only expanded proxies that
;; were at the first position in the argument list. The implementation
;; below is less general in that one must specifically enable proxying
;; for a class by calling ENABLE-LAZY-INIT. This generates the
;; necessary portable methods on the NO-APPLICABLE-METHOD generic
;; function to enable the proxy to function for that class

(defclass lazy-init-proxy ()
  ((proxied-class :initarg :proxied-class
                  :reader proxied-class-of
                  :documentation "The class-name of class of the
object to be proxied.")
   (initargs :initarg :initargs
             :reader initargs-of
             :documentation "A list of the initargs for the object to
be proxied."))
  (:documentation "A lazy initialization proxy class. Each instance
retains the class-name and initialization arguments sufficient to
lazily create an object of the proxied class."))

(defgeneric reify (lazy-init-proxy)
  (:documentation "Creates a concrete object from LAZY-INIT-PROXY
using the meta-information about the object contained in the proxy's
slots."))

(defmethod reify ((p lazy-init-proxy))
  (with-slots (proxied-class initargs) p
    (let ((initargs initargs))
      (change-class p proxied-class)
      (apply #'initialize-instance p initargs))))

(defmacro define-lazy-init-method (generic-function)
  "Defines no-applicable-method with an eql specifier on
GENERIC-FUNCTION such that if any of the arguments supplied to a call
to no-applicable-method are instances of lazy-init-proxy, those
instances are converted to appropriately initialized instances of the
proxied class."
  `(defmethod no-applicable-method ((f (eql ,generic-function))
                                    &rest args)
     (let ((proxies (mapcar #'(lambda (arg)
                                (typep arg 'lazy-init-proxy)) args)))
       (if (or proxies)
            (apply f (mapcar #'(lambda (proxyp arg)
                                 (when proxyp
                                   (reify arg))) proxies args))
          (call-next-method)))))

(defun enable-lazy-init (class)
  "Enables lazy initialization proxy support for CLASS."
  (dolist (gf (all-specialized-generic-functions class
                                                 (find-class
                                                  'standard-object)))
    (define-lazy-init-method gf)))
