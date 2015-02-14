(in-package :cl-user)
(defpackage :cl-binary.io
  (:use :cl
        :trivial-gray-streams))
(in-package :cl-binary.io)

(defclass binary-input-stream (fundamental-binary-input-stream)
  ((data :initarg :data :type (vector (unsigned-byte 8) (*)))
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
  ((data :initform nil :initarg :data :type (vector (unsigned-byte 8) (*)))))

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

(defmacro def-signed (name unit place &optional val &key (direction :input))
  (let ((sname (intern (format nil "~a-S~a" name unit)))
        (uname (intern (format nil "~a-U~a" name unit))))
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
  `(defun ,(intern (format nil "READ-U~a" unit)) (stream)
     ,(if (<= unit 8)
          `(read-byte stream)
          `(logior
            ,@(loop for i from 0 below unit by 8
                 collect `(ash (read-byte stream) ,i))))))

(def-read-u* 8)
(def-read-u* 16)
(def-read-u* 32)
(def-read-u* 64)

;;; read-s8
;;; read-s16
;;; read-s32
;;; read-s64
(def-signed read   8 (stream) nil :direction :input)
(def-signed read  16 (stream) nil :direction :input)
(def-signed read  32 (stream) nil :direction :input)
(def-signed read  64 (stream) nil :direction :input)

;;; write-u8
;;; write-u16
;;; write-u32
;;; write-u64
(defmacro def-write-u* (unit)
  `(defun ,(intern (format nil "WRITE-U~a" unit)) (stream val)
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
(def-signed write  8 (stream) val :direction :output)
(def-signed write 16 (stream) val :direction :output)
(def-signed write 32 (stream) val :direction :output)
(def-signed write 64 (stream) val :direction :output)

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
                (U 'unsigned-byte)
                (S 'signed-byte))))
    `(defmacro ,(intern (format nil "MAKE-~a~aVECTOR" sign unit)) (len &optional (fill 0))
       `(make-array ,len :element-type '(,',type ,',unit) :initial-element ,fill))))

(make-uvector u 8)
(make-uvector u 16)
(make-uvector u 32)
(make-uvector u 64)
(make-uvector s 8)
(make-uvector s 16)
(make-uvector s 32)
(make-uvector s 64)

;;; u8vector
;;; u16vector
;;; u32vector
;;; u64vector
;;; s8vector
;;; s16vector
;;; s32vector
;;; s64vector
(defmacro uvector (sign unit)
  (let ((type (case sign
                (U 'unsigned-byte)
                (S 'signed-byte))))
    `(defmacro ,(intern (format nil "~a~aVECTOR" sign unit)) (&rest objects)
       `(make-array (length ',objects) :element-type '(,',type ,',unit) :initial-contents ',objects))))

(uvector u 8)
(uvector u 16)
(uvector u 32)
(uvector u 64)
(uvector s 8)
(uvector s 16)
(uvector s 32)
(uvector s 64)

;;; read-u8vector
;;; read-u16vector
;;; read-u32vector
;;; read-u64vector
;;; read-s8vector
;;; read-s16vector
;;; read-s32vector
;;; read-s64vector
(defmacro def-read-uvector (sign unit)
  `(defun ,(intern (format nil "READ-~a~aVECTOR" sign unit)) (len stream)
     (loop
        with uv = (,(intern (format nil "MAKE-~a~aVECTOR" sign unit)) len)
        for i below len
        do (setf (aref uv i) (,(intern (format nil "READ-~a~a" sign unit)) stream))
        finally (return uv))))

(def-read-uvector u 8)
(def-read-uvector u 16)
(def-read-uvector u 32)
(def-read-uvector u 64)
(def-read-uvector s 8)
(def-read-uvector s 16)
(def-read-uvector s 32)
(def-read-uvector s 64)

;;; write-u8vector
;;; write-u16vector
;;; write-u32vector
;;; write-u64vector
;;; write-s8vector
;;; write-s16vector
;;; write-s32vector
;;; write-s64vector
(defmacro def-write-uvector (sign unit)
  `(defun ,(intern (format nil "WRITE-~a~aVECTOR" sign unit)) (stream vec)
     (loop for x across vec
        do (,(intern (format nil "WRITE-~a~a" sign unit)) stream x))))

(def-write-uvector u 8)
(def-write-uvector u 16)
(def-write-uvector u 32)
(def-write-uvector u 64)
(def-write-uvector s 8)
(def-write-uvector s 16)
(def-write-uvector s 32)
(def-write-uvector s 64)

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
  `(defun ,(intern (format nil "GET-U~a" unit)) (uv pos)
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
(def-signed get  8 (uv pos) nil :direction :input)
(def-signed get 16 (uv pos) nil :direction :input)
(def-signed get 32 (uv pos) nil :direction :input)
(def-signed get 64 (uv pos) nil :direction :input)

;;; put-u8
;;; put-u16
;;; put-u32
;;; put-u64
(defmacro def-put-u* (unit)
  `(defun ,(intern (format nil "PUT-U~a" unit)) (uv pos val)
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
(def-signed put  8 (uv pos) val :direction :output)
(def-signed put 16 (uv pos) val :direction :output)
(def-signed put 32 (uv pos) val :direction :output)
(def-signed put 64 (uv pos) val :direction :output)
