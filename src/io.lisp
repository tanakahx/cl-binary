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

(defmacro with-open-binary-input-file ((stream filespec) &body body)
  `(with-open-file (,stream ,filespec :element-type '(unsigned-byte 8))
     ,@body))

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

(defmacro with-open-binary-output-file ((stream filespec) &body body)
  `(with-open-file (,stream ,filespec :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
     ,@body))

(defun read-u8 (stream)
  (read-byte stream))

(defun read-u16 (stream)
  (logior (ash (read-byte stream) 0)
          (ash (read-byte stream) 8)))

(defun read-u32 (stream)
  (logior (ash (read-byte stream) 0)
          (ash (read-byte stream) 8)
          (ash (read-byte stream) 16)
          (ash (read-byte stream) 24)))

(defun write-u8 (data stream)
  (write-byte data stream))

(defun write-u16 (data stream)
  (write-byte (logand #xff (ash data  0)) stream)
  (write-byte (logand #xff (ash data -8)) stream))

(defun write-u32 (data stream)
  (write-byte (logand #xff (ash data   0)) stream)
  (write-byte (logand #xff (ash data  -8)) stream)
  (write-byte (logand #xff (ash data -16)) stream)
  (write-byte (logand #xff (ash data -24)) stream))

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

(defun write-s8 (data stream)
  (write-u8 (s-to-u data 8) stream))

(defun write-s16 (data stream)
  (write-u16 (s-to-u data 16) stream))

(defun write-s32 (data stream)
  (write-u32 (s-to-u data 32) stream))
