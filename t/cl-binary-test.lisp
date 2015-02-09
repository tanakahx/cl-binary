(in-package :cl-user)
(defpackage :cl-binary-test
  (:use :cl
        :cl-binary
        :fiveam))
(in-package :cl-binary-test)

(def-suite cl-binary-test)
(in-suite cl-binary-test)

(test read-u*
  (with-binary-input-stream (in #(#x12 #x34 #x56 #x78))
    (is (and (= (read-u8 in) #x12)
             (= (read-u8 in) #x34)
             (= (read-u8 in) #x56)
             (= (read-u8 in) #x78))))
  (with-binary-input-stream (in #(#x12 #x34 #x56 #x78))
    (is (and (= (read-u16 in) #x3412)
             (= (read-u16 in) #x7856))))
  (with-binary-input-stream (in #(#x12 #x34 #x56 #x78))
    (is (= (read-u32 in) #x78563412))))

(test write-u*
  (is (equalp (with-binary-output-stream (out)
                (write-u8 #x01 out)
                (write-u8 #x23 out)
                (write-u8 #x45 out)
                (write-u8 #x67 out))
              #(#x01 #x23 #x45 #x67)))
  (is (equalp (with-binary-output-stream (out)
                (write-u16 #x2301 out)
                (write-u16 #x6745 out))
              #(#x01 #x23 #x45 #x67)))
  (is (equalp (with-binary-output-stream (out)
                (write-u32 #x67452301 out)
                (write-u32 #xefcdab89 out))
              #(#x01 #x23 #x45 #x67
                #x89 #xab #xcd #xef))))

(test read-s*
  (with-binary-input-stream (in #(#x80 #xff #xff #xff))
    (is (= (read-s8 in) -128)))
  (with-binary-input-stream (in #(#x00 #x80 #xff #xff))
    (is (= (read-s16 in) -32768)))
  (with-binary-input-stream (in #(#x00 #x00 #x00 #x80))
    (is (= (read-s32 in) -2147483648))))

(test write-s*
  (is (equalp (with-binary-output-stream (out)
                (write-s8 -128 out)
                (write-s8 -127 out)
                (write-s8 -126 out)
                (write-s8 -125 out))
              #(#x80 #x81 #x82 #x83)))
  (is (equalp (with-binary-output-stream (out)
                (write-s16 -32768 out)
                (write-s16 -32767 out)
                (write-s16 -32766 out)
                (write-s16 -32765 out))
              #(#x00 #x80 #x01 #x80
                #x02 #x80 #x03 #x80)))
  (is (equalp (with-binary-output-stream (out)
                (write-s32 -2147483648 out)
                (write-s32 -2147483647 out)
                (write-s32 -2147483646 out)
                (write-s32 -2147483645 out))
              #(#x00 #x00 #x00 #x80
                #x01 #x00 #x00 #x80
                #x02 #x00 #x00 #x80
                #x03 #x00 #x00 #x80))))
