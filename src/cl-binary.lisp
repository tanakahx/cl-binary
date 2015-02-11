(in-package :cl-user)
(defpackage :cl-binary
  (:use :cl
        :trivial-gray-streams
        :cl-binary.io)
  (:import-from :cl-binary.io
                :make-binary-input-stream
                :make-binary-output-stream
                :with-binary-input-stream
                :with-binary-output-stream
                :with-open-binary-input-file
                :with-open-binary-output-file
                :read-u8
                :read-u16
                :read-u32
                :read-u64
                :write-u8
                :write-u16
                :write-u32
                :write-u64
                :read-s8
                :read-s16
                :read-s32
                :read-s64
                :write-s8
                :write-s16
                :write-s32
                :write-s64)
  (:export :make-binary-input-stream
           :make-binary-output-stream
           :with-binary-input-stream
           :with-binary-output-stream
           :with-open-binary-input-file
           :with-open-binary-output-file
           :read-u8
           :read-u16
           :read-u32
           :read-u64
           :write-u8
           :write-u16
           :write-u32
           :write-u64
           :read-s8
           :read-s16
           :read-s32
           :read-s64
           :write-s8
           :write-s16
           :write-s32
           :write-s64))
(in-package :cl-binary)
