
(in-package :cl-gp-utilities-system)

(fiveam:def-suite testsuite
    :description "The test suite.")


(in-package :cl-gp-utilities-test)

(defclass x1 ()
  ((slotx1 :initarg :slotx1
           :reader slotx1-of)))

(defclass y1 ()
  ((sloty1 :initarg :sloty1
           :reader sloty1-of)))

(defclass x2 (x1)
  ((slotx2 :initarg :slotx2
           :reader slotx2-of)))

(defclass x3 (x2 y1)
  ((slotx3 :initarg :slotx3
           :reader slotx3-of)))

(enable-lazy-init (find-class 'x3))

(defun find-slot-reader-method (class-name generic-function)
  (find-method generic-function '() (list (find-class class-name))))

(in-suite cl-gp-utilities-system:testsuite)

(test all-superclasses/one-arg
  "Test ALL-SUPERCLASSES."
  (is (equal (list (find-class 'x1))
             (all-superclasses (find-class 'x1))))
  (is (equal (mapcar #'find-class '(x1 x2))
             (all-superclasses (find-class 'x2))))
  (is (equal (mapcar #'find-class '(y1 x1 x2 x3))
             (all-superclasses (find-class 'x3)))))

(test all-specialized-methods
  "Test ALL-SPECIALIZED-METHODS."
  (is (subsetp (mapcar #'find-slot-reader-method
                       '(x1) (list #'slotx1-of))
               (all-specialized-methods (find-class 'x1))))
  (is (subsetp (mapcar #'find-slot-reader-method
                       '(x1 x2) (list #'slotx1-of #'slotx2-of))
               (all-specialized-methods (find-class 'x2))))
  (is (subsetp (mapcar #'find-slot-reader-method
                       '(x1 x2 x3 y1) (list #'slotx1-of #'slotx2-of
                                            #'slotx3-of #'sloty1-of))
               (all-specialized-methods (find-class 'x3)))))

(test all-specialized-generic-functions
  "Test ALL-SPECIALIZED-GENERIC-FUNCTIONS."
  (is (subsetp (mapcar #+:sbcl #'sb-mop:method-generic-function
                       #+:cmu #'mop:method-generic-function
                       (all-specialized-methods (find-class 'x1)))
               (all-specialized-generic-functions
                (find-class 'x1))))
  (is (subsetp (mapcar #+:sbcl #'sb-mop:method-generic-function
                       #+:cmu #'mop:method-generic-function
                       (all-specialized-methods (find-class 'x2)))
               (all-specialized-generic-functions
                (find-class 'x2))))
  (is (subsetp (mapcar #+:sbcl #'sb-mop:method-generic-function
                       #+:cmu #'mop:method-generic-function
                       (all-specialized-methods (find-class 'x3)))
               (all-specialized-generic-functions
                (find-class 'x3)))))

(test lazy-init
  "Test lazy initialization of LAZY-INIT-PROXY."
  (let ((proxy (make-instance 'lazy-init-proxy
                              :proxied-class 'x3
                              :initargs (list :slotx1 'sx1
                                              :slotx2 'sx2
                                              :slotx3 'sx3
                                              :sloty1 'sy1))))
    (is (find-class 'lazy-init-proxy)
        (class-of proxy))
    (is (eql 'sx1 (slotx1-of proxy)))
    (is (find-class 'x1)
        (class-of proxy))))
