
(in-package :cl-gp-utilities-system)

(fiveam:def-suite testsuite
    :description "The test suite.")


(in-package :cl-gp-utilities-test)

(defclass x1 ()
  ((slotx1 :initarg :slotx1
           :accessor slotx1-of)))

(defclass y1 ()
  ((sloty1 :initarg :sloty1
           :accessor sloty1-of)))

(defclass x2 (x1)
  ((slotx2 :initarg :slotx2
           :accessor slotx2-of)))

(defclass x3 (x2 y1)
  ((slotx3 :initarg :slotx3
           :accessor slotx3-of)))

(enable-lazy-init (find-class 'x3))

(in-suite cl-gp-utilities-system:testsuite)

(test all-superclasses/one-arg
  "Test ALL-SUPERCLASSES."
  (is (find-class 'x1)
      (all-superclasses 'x1))
  (is (mapcar #'find-class '(x1 x2))
      (all-superclasses (find-class 'x2)))
  (is (mapcar #'find-class '(y1 x1 x2 x3))
      (all-superclasses (find-class 'x3))))

(test all-specialized-methods/counts
  "Test ALL-SPECIALIZED-METHODS. At the moment only a count."
  (is (= 2 (length (all-specialized-methods (find-class 'x1)))))
  (is (= 4 (length (all-specialized-methods (find-class 'x2)))))
  (is (= 8 (length (all-specialized-methods (find-class 'x3))))))

(test all-specialized-generic-functions/counts
  "Test ALL-SPECIALIZED-GENERIC-FUNCTIONS. At the moment only a
count."
  (is (= 2 (length (all-specialized-generic-functions
                    (find-class 'x1)))))
  (is (= 4 (length (all-specialized-generic-functions
                    (find-class 'x2)))))
  (is (= 8 (length (all-specialized-generic-functions
                    (find-class 'x3))))))

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
