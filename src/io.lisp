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

;;; write-u8
;;; write-u16
;;; write-u32
;;; write-u64
(defmacro def-write-u* (unit)
  `(defun ,(intern (format nil "WRITE-U~a" unit)) (data stream)
     ,(if (<= unit 8)
          `(write-byte data stream)
          (cons 'progn
                (loop for i from 0 above (- unit) by 8
                   collect `(write-byte (logand #xff (ash data ,i)) stream))))))

(def-write-u* 8)
(def-write-u* 16)
(def-write-u* 32)
(def-write-u* 64)

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

(defun read-s8 (stream)
  (u-to-s (read-u8 stream) 8))

(defun read-s16 (stream)
  (u-to-s (read-u16 stream) 16))

(defun read-s32 (stream)
  (u-to-s (read-u32 stream) 32))

(defun read-s64 (stream)
  (u-to-s (read-u64 stream) 64))

(defun write-s8 (data stream)
  (write-u8 (s-to-u data 8) stream))

(defun write-s16 (data stream)
  (write-u16 (s-to-u data 16) stream))

(defun write-s32 (data stream)
  (write-u32 (s-to-u data 32) stream))

(defun write-s64 (data stream)
  (write-u64 (s-to-u data 64) stream))

;;; read-u8vector
;;; read-u16vector
;;; read-u32vector
;;; read-u64vector
(defmacro def-read-uvector (unit)
  `(defun ,(intern (format nil "READ-U~aVECTOR" unit)) (size stream)
     (loop
        with uv = (make-array size :element-type '(unsigned-byte ,unit))
        for i below size
        do (setf (aref uv i) (,(intern (format nil "READ-U~a" unit)) stream))
        finally (return uv))))

(def-read-uvector 8)
(def-read-uvector 16)
(def-read-uvector 32)
(def-read-uvector 64)

;;; Define the following functions
;;; write-u8vector
;;; write-u16vector
;;; write-u32vector
;;; write-u64vector
(defmacro def-write-uvector (unit)
  `(defun ,(intern (format nil "WRITE-U~aVECTOR" unit)) (vec stream)
     (loop for x across vec
        do (,(intern (format nil "WRITE-U~a" unit)) x stream))))

(def-write-uvector 8)
(def-write-uvector 16)
(def-write-uvector 32)
(def-write-uvector 64)

(defun uvector-to-string (uv)
  (loop
     with s = (make-array (length uv) :element-type 'character)
     for i from 0 below (length uv)
     do (setf (schar s i) (code-char (aref uv i)))
     finally (return s)))
