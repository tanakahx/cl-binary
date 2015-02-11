(in-package :cl-user)
(defpackage :cl-binary-test
  (:use :cl
        :cl-binary
        :fiveam))
(in-package :cl-binary-test)

(def-suite cl-binary-test)
(in-suite cl-binary-test)

(test read-u*
  (with-binary-input-stream (in #(#x01 #x23 #x45 #x67))
    (is (and (= (read-u8 in) #x01)
             (= (read-u8 in) #x23)
             (= (read-u8 in) #x45)
             (= (read-u8 in) #x67))))
  (with-binary-input-stream (in #(#x01 #x23 #x45 #x67))
    (is (and (= (read-u16 in) #x2301)
             (= (read-u16 in) #x6745))))
  (with-binary-input-stream (in #(#x01 #x23 #x45 #x67))
    (is (= (read-u32 in) #x67452301)))
  (with-binary-input-stream (in #(#x01 #x23 #x45 #x67 #x89 #xab #xcd #xef))
    (is (= (read-u64 in) #xefcdab8967452301))))

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
                #x89 #xab #xcd #xef)))
  (is (equalp (with-binary-output-stream (out)
                (write-u64 #xefcdab8967452301 out))
              #(#x01 #x23 #x45 #x67
                #x89 #xab #xcd #xef))))

(test read-s*
  (with-binary-input-stream (in #(#x80 #xff #xff #xff))
    (is (= (read-s8 in) (- (expt 2 7)))))
  (with-binary-input-stream (in #(#x00 #x80 #xff #xff))
    (is (= (read-s16 in) (- (expt 2 15)))))
  (with-binary-input-stream (in #(#x00 #x00 #x00 #x80))
    (is (= (read-s32 in) (- (expt 2 31)))))
  (with-binary-input-stream (in #(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80))
    (is (= (read-s64 in) (- (expt 2 63))))))

(test write-s*
  (is (equalp (with-binary-output-stream (out)
                (write-s8 (+ (- (expt 2 7)) 0) out)
                (write-s8 (+ (- (expt 2 7)) 1) out)
                (write-s8 (+ (- (expt 2 7)) 2) out)
                (write-s8 (+ (- (expt 2 7)) 3) out))
              #(#x80 #x81 #x82 #x83)))
  (is (equalp (with-binary-output-stream (out)
                (write-s16 (+ (- (expt 2 15)) 0) out)
                (write-s16 (+ (- (expt 2 15)) 1) out)
                (write-s16 (+ (- (expt 2 15)) 2) out)
                (write-s16 (+ (- (expt 2 15)) 3) out))
              #(#x00 #x80 #x01 #x80
                #x02 #x80 #x03 #x80)))
  (is (equalp (with-binary-output-stream (out)
                (write-s32 (+ (- (expt 2 31)) 0) out)
                (write-s32 (+ (- (expt 2 31)) 1) out)
                (write-s32 (+ (- (expt 2 31)) 2) out)
                (write-s32 (+ (- (expt 2 31)) 3) out))
              #(#x00 #x00 #x00 #x80
                #x01 #x00 #x00 #x80
                #x02 #x00 #x00 #x80
                #x03 #x00 #x00 #x80)))
  (is (equalp (with-binary-output-stream (out)
                (write-s64 (+ (- (expt 2 63)) 0) out)
                (write-s64 (+ (- (expt 2 63)) 1) out)
                (write-s64 (+ (- (expt 2 63)) 2) out)
                (write-s64 (+ (- (expt 2 63)) 3) out))
              #(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                #x02 #x00 #x00 #x00 #x00 #x00 #x00 #x80
                #x03 #x00 #x00 #x00 #x00 #x00 #x00 #x80))))
