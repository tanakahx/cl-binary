(in-package :cl-user)
(defpackage :cl-binary.io
  (:use :cl
        :trivial-gray-streams
        :ieee-floats))
(in-package :cl-binary.io)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args)
        (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))

(defclass binary-input-stream (fundamental-binary-input-stream)
  ((data :initarg :data :type (vector (unsigned-byte 8) *))
   (index :initarg :index)
   (end :initarg :end)))

(defun make-binary-input-stream (data)
  (make-instance 'binary-input-stream :data data :index 0 :end (length data)))

(defmethod stream-read-byte ((stream fundamental-binary-input-stream))
  (with-slots (data index end) stream
    (if (>= index end)
        :eof
        (prog1 (svref data index)
          (incf index)))))

(defmacro with-binary-input-stream ((stream vector) &body body)
  `(let ((,stream (make-binary-input-stream ,vector)))
     (unwind-protect
          (progn ,@body)
       (close ,stream))))

(defclass binary-output-stream (fundamental-binary-output-stream)
  ((data :initform (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)
         :initarg :data
         :type (vector (unsigned-byte 8) *))))

(defun make-binary-output-stream (&optional data)
  (make-instance 'binary-output-stream :data data))

(defmethod stream-write-byte ((stream fundamental-binary-output-stream) datum)
  (with-slots (data) stream
    (when (null data)
      (setf data (make-array 64 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (vector-push-extend datum data)
    datum))

(defmacro with-binary-output-stream ((stream &optional vector) &body body)
  `(let ((,stream (make-binary-output-stream ,vector)))
     (unwind-protect
          (progn ,@body
                 (slot-value ,stream 'data))
       (close ,stream))))

(defmacro with-open-binary-file ((stream filespec &key (direction :input) (if-exists :supersede)) &body body)
  `(with-open-file (,stream ,filespec :direction ,direction :if-exists ,if-exists :element-type '(unsigned-byte 8))
     ,@body))

(defun u-to-s (number bit)
  "Convert an unsigned number to a signed number with `bit` length."
  (if (and (plusp number)
           (< number (ash 1 bit)))
      (if (plusp (logand number (ash 1 (1- bit))))
          (- number (ash 1 bit))
          number)
      (error "Out of bounds error (Number is beyond ~a bit)" bit)))

(defun s-to-u (number bit)
  "Convert a signed number to an unsigned number with `bit` length."
  (if (and (<= (- (ash 1 (1- bit))) number)
           (< number (ash 1 (1- bit))))
      (if (minusp number)
          (+ number (ash 1 bit))
          number)
      (error "Out of bounds error (Number is beyond ~a bit)" bit)))

(defmacro def-signed (name unit place &key direction val)
  (let ((sname (symb name "-S" unit))
        (uname (symb name "-U" unit)))
    (case direction
      (:input
       `(defun ,sname (,@place)
          (u-to-s (,uname ,@place) ,unit)))
      (:output
       `(defun ,sname (,@place ,val)
          (,uname ,@place (s-to-u ,val ,unit))))
      (t (error "Unknown direction: ~a~%" direction)))))

;;; read-u8
;;; read-u16
;;; read-u32
;;; read-u64
(defmacro def-read-u* (unit)
  `(defun ,(symb "READ-U" unit) (stream)
     ,(if (<= unit 8)
          `(read-byte stream nil nil)
          (let ((b (gensym)))
            `(let ((,b (read-byte stream nil nil)))
               (when ,b
                 (logior
                  ,b
                  ,@(loop for i from 8 below unit by 8
                       collect `(ash (read-byte stream nil 0) ,i)))))))))

(def-read-u* 8)
(def-read-u* 16)
(def-read-u* 32)
(def-read-u* 64)

;;; read-s8
;;; read-s16
;;; read-s32
;;; read-s64
(def-signed read   8 (stream) :direction :input)
(def-signed read  16 (stream) :direction :input)
(def-signed read  32 (stream) :direction :input)
(def-signed read  64 (stream) :direction :input)

;;; read-f32
;;; read-f64
(defmacro def-read-f* (unit)
  `(defun ,(symb "READ-F" unit) (stream)
     (,(symb "DECODE-FLOAT" unit) (,(symb "READ-U" unit)  stream))))

(def-read-f* 32)
(def-read-f* 64)

;;; write-u8
;;; write-u16
;;; write-u32
;;; write-u64
(defmacro def-write-u* (unit)
  `(defun ,(symb "WRITE-U" unit) (stream val)
     ,(if (<= unit 8)
          `(write-byte val stream)
          (cons 'progn
                (loop for i from 0 above (- unit) by 8
                   collect `(write-byte (logand #xff (ash val ,i)) stream))))))

(def-write-u* 8)
(def-write-u* 16)
(def-write-u* 32)
(def-write-u* 64)

;;; write-s8
;;; write-s16
;;; write-s32
;;; write-s64
(def-signed write  8 (stream) :direction :output :val val)
(def-signed write 16 (stream) :direction :output :val val)
(def-signed write 32 (stream) :direction :output :val val)
(def-signed write 64 (stream) :direction :output :val val)

;;; write-f32
;;; write-f64
(defmacro def-write-f* (unit)
  `(defun ,(symb "WRITE-F" unit) (stream val)
     (,(symb "WRITE-U" unit) stream (,(symb "ENCODE-FLOAT" unit) val))))

(def-write-f* 32)
(def-write-f* 64)

;;; make-u8vector
;;; make-u16vector
;;; make-u32vector
;;; make-u64vector
;;; make-s8vector
;;; make-s16vector
;;; make-s32vector
;;; make-s64vector
(defmacro make-uvector (sign unit)
  (let ((type (case sign
                (U `'(unsigned-byte ,unit))
                (S `'(signed-byte ,unit))
                (F (if (= unit 32)
                       ''single-float
                       ''double-float)))))
    `(defmacro ,(symb "MAKE-" sign unit "VECTOR") (len &optional (fill nil supplied-p))
       (if supplied-p
           `(make-array ,len :element-type ,',type :initial-element ,fill)
           `(make-array ,len :element-type ,',type)))))

(make-uvector u 8)
(make-uvector u 16)
(make-uvector u 32)
(make-uvector u 64)
(make-uvector s 8)
(make-uvector s 16)
(make-uvector s 32)
(make-uvector s 64)
(make-uvector f 32)
(make-uvector f 64)

;;; u8vector
;;; u16vector
;;; u32vector
;;; u64vector
;;; s8vector
;;; s16vector
;;; s32vector
;;; s64vector
;;; f32vector
;;; f64vector
(defmacro uvector (sign unit)
  (let ((type (case sign
                (U `'(unsigned-byte ,unit))
                (S `'(signed-byte ,unit))
                (F (if (= unit 32)
                       ''single-float
                       ''double-float)))))
    `(defmacro ,(symb sign unit "VECTOR") (&rest objects)
       `(make-array (length ',objects) :element-type ,',type :initial-contents ',objects))))

(uvector u 8)
(uvector u 16)
(uvector u 32)
(uvector u 64)
(uvector s 8)
(uvector s 16)
(uvector s 32)
(uvector s 64)
(uvector f 32)
(uvector f 64)

;;; read-u8vector
;;; read-u16vector
;;; read-u32vector
;;; read-u64vector
;;; read-s8vector
;;; read-s16vector
;;; read-s32vector
;;; read-s64vector
;;; read-f32vector
;;; read-f64vector
(defmacro def-read-uvector (sign unit)
  `(defun ,(symb "READ-" sign unit "VECTOR") (len stream)
     (loop
        with uv = (,(symb "MAKE-" sign unit "VECTOR") len)
        for i below len
        do (setf (aref uv i) (,(symb "READ-" sign unit) stream))
        finally (return uv))))

(def-read-uvector u 8)
(def-read-uvector u 16)
(def-read-uvector u 32)
(def-read-uvector u 64)
(def-read-uvector s 8)
(def-read-uvector s 16)
(def-read-uvector s 32)
(def-read-uvector s 64)
(def-read-uvector f 32)
(def-read-uvector f 64)

;;; write-u8vector
;;; write-u16vector
;;; write-u32vector
;;; write-u64vector
;;; write-s8vector
;;; write-s16vector
;;; write-s32vector
;;; write-s64vector
;;; write-f32vector
;;; write-f64vector
(defmacro def-write-uvector (sign unit)
  `(defun ,(symb "WRITE-" sign unit "VECTOR") (stream vec)
     (loop for x across vec
        do (,(symb "WRITE-" sign unit) stream x))))

(def-write-uvector u 8)
(def-write-uvector u 16)
(def-write-uvector u 32)
(def-write-uvector u 64)
(def-write-uvector s 8)
(def-write-uvector s 16)
(def-write-uvector s 32)
(def-write-uvector s 64)
(def-write-uvector f 32)
(def-write-uvector f 64)

(defun uvector-to-string (uv)
  (loop
     with s = (make-array (length uv) :element-type 'character)
     for i from 0 below (length uv)
     do (setf (schar s i) (code-char (aref uv i)))
     finally (return s)))

;;; get-u8
;;; get-u16
;;; get-u32
;;; get-u64
(defmacro def-get-u* (unit)
  `(defun ,(symb "GET-U" unit) (uv pos)
     ,(if (<= unit 8)
          `(aref uv pos)
          `(logior
            ,@(loop
                 for i from 0 below unit by 8
                 for p from 0
                 collect `(ash (aref uv (+ pos ,p)) ,i))))))

(def-get-u* 8)
(def-get-u* 16)
(def-get-u* 32)
(def-get-u* 64)

;;; get-s8
;;; get-s16
;;; get-s32
;;; get-s64
(def-signed get  8 (uv pos) :direction :input)
(def-signed get 16 (uv pos) :direction :input)
(def-signed get 32 (uv pos) :direction :input)
(def-signed get 64 (uv pos) :direction :input)

;;; get-f32
;;; get-f64
(defmacro def-get-f* (unit)
  `(defun ,(symb "GET-F" unit) (uv pos)
     (decode-float32
      (logior
       ,@(loop
            for i from 0 below unit by 8
            for p from 0
            collect `(ash (aref uv (+ pos ,p)) ,i))))))
(def-get-f* 32)
(def-get-f* 64)

;;; put-u8
;;; put-u16
;;; put-u32
;;; put-u64
(defmacro def-put-u* (unit)
  `(defun ,(symb "PUT-U" unit) (uv pos val)
     ,(if (<= unit 8)
          `(setf (aref uv pos) val)
          (cons 'progn
                (loop
                   for i from 0 above (- unit) by 8
                   for p from 0
                   collect `(setf (aref uv (+ pos ,p)) (logand #xff (ash val ,i))))))))

(def-put-u* 8)
(def-put-u* 16)
(def-put-u* 32)
(def-put-u* 64)

;;; put-s8
;;; put-s16
;;; put-s32
;;; put-s64
(def-signed put  8 (uv pos) :direction :output :val val)
(def-signed put 16 (uv pos) :direction :output :val val)
(def-signed put 32 (uv pos) :direction :output :val val)
(def-signed put 64 (uv pos) :direction :output :val val)

;;; put-f32
;;; put-f64
(defmacro def-put-f* (unit)
  `(defun ,(symb "PUT-F" unit) (uv pos val)
     (let ((encoded-val (,(symb "ENCODE-FLOAT" unit) val)))
       ,@(loop
            for i from 0 above (- unit) by 8
            for p from 0
            collect `(setf (aref uv (+ pos ,p)) (logand #xff (ash encoded-val ,i)))))))

(def-put-f* 32)
(def-put-f* 64)
