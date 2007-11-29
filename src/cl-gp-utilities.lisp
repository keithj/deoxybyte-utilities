
(in-package :cl-gp-utilities)

(deftype array-subscript ()
  '(and fixnum (integer 0 *)))

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
