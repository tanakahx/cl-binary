# CL-BINARY

A binary stream processing library for Common Lisp

CL-BINARY makes it possible to read and write binary files more easily in Common Lisp.
This library opens a stream for a binary file and allows access in 8, 16, 32, and 64 bit units, signed or unsigned.
It has also useful 'with-' macros like `with-open-file` and `with-input-from-string` to create a binary I/O stream for a given file or vector.
The interface design of this library is derived from Gauche Scheme interpreter.

## Usage

### How to load CL-BINARY
Clone this repository to a suitable location and add the path to it in ASDF source-registry.

```shell
$ cd ~/src/github.com/tanakahx
$ git clone https://github.com/tanakahx/cl-binary
$ cat ~/.config/common-lisp/source-registry.conf
(:source-registry
  (:tree (:home "common-lisp"))
  (:directory (:home "src/github.com/tanakahx/" :*/))
  :inherit-configuration)
```

Since CL-BINARY depends on the `trivial-gray-streams` and `ieee-floats` libraries,
you should get their source codes and add them to ASDF source-registry as well
or install them using Quicklisp.

Finally, load CL-BINARY with `asdf:load-system` on REPL.

```cl
(asdf:load-system :cl-binary)
```

### with-open-binary-file stream filespec
Open file in binary mode and write data to it.
```cl
(cl-binary:with-open-binary-file (out "foo.bin" :direction :output)
  (cl-binary:write-u8  out #x01)
  (cl-binary:write-u16 out #x0123)
  (cl-binary:write-u32 out #x01234567)
  (cl-binary:write-u64 out #x0123456789abcdef))
```

Open binary file and read data from it.
```cl
(cl-binary:with-open-binary-file (in "foo.bin")
  (format t "0x~X~%" (cl-binary:read-u8 in))
  (format t "0x~X~%" (cl-binary:read-u16 in))
  (format t "0x~X~%" (cl-binary:read-u32 in))
  (format t "0x~X~%" (cl-binary:read-u64 in)))
;; 0x1
;; 0x123
;; 0x1234567
;; 0x123456789ABCDEF
```

### with-binary-output-stream stream &optional vector
Open an output stream and write to it as binary data.
```cl
(cl-binary:with-binary-output-stream (out)
  (cl-binary:write-u32 out #x67452301)) ; => #(#x01 #x23 #x45 #x67)

(let ((arr (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#x01 #x23 #x45 #x67) :fill-pointer t :adjustable t)))
  (cl-binary:with-binary-output-stream (out arr)
    (cl-binary:write-u32 out #xefcdab89))) ; => #(#x01 #x23 #x45 #x67 #x89 #xab #xcd #xef)
```

### with-binary-input-stream stream vector
Open an input stream and read from it as binary data.
```cl
(cl-binary:with-binary-input-stream (in #(#x01 #x23 #x45 #x67 #x89 #xab #xcd #xef))
  (format t "0x~X~%" (cl-binary:read-u8 in))
  (format t "0x~X~%" (cl-binary:read-u8 in))
  (format t "0x~X~%" (cl-binary:read-u16 in))
  (format t "0x~X~%" (cl-binary:read-u32 in)))
;; 0x1
;; 0x23
;; 0x6745
;; 0xEFCDAB89
```
