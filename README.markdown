# CL-BINARY

A binary stream processing library for Common Lisp

This library is developed to make it more easy to read and write binary files in Common Lisp. It can access binary files via binary streams and its access units are 8, 16, 32 and 64 bits. It can be specified to treat binary data as unsigned integer or signed integer.
It also has convenient 'with-' macros which create a binary input or output stream of a specified file or a vector like `with-open-file` or `with-input-from-string`.

The interface design is derived from Gauche Scheme interpreter.

## Usage

### How to load cl-binary
Add the path to the cloned repository into the ASDF source-registry and load cl-binary like this.
```cl
(asdf:load-system :cl-binary)
```

### with-open-binary-file stream filespec
Open binary file and read binary data.
```cl
(with-open-binary-file (in "foo.bin")
  (format t "~A~%" (read-u8 in))
  (format t "~A~%" (read-u16 in))
  (foramt t "~A~%" (read-u32 in))
  (foramt t "~A~%" (read-u64 in)))
```

```cl
(with-open-binary-file (out "foo.bin" :direction :output)
  (write-u8  out #x01)
  (write-u16 out #x23)
  (write-u32 out #x45)
  (write-u64 out #x67))
```

### with-open-input-binary-stream stream filespec
Open binary stream for reading from vector and read binary data from it.
```cl
(with-open-input-binary-stream (in #(#x01 #x23 #x45 #x67))
  (foramt t "~A~%" (read-u32 in))) ; => #x67452301
```

### with-open-output-binary-stream
Open binary stream for writing to vector and write binary data to it.
```cl
(with-open-output-binary-stream (out)
  (write-u32 #x67452301)) ; => #(#x01 #x23 #x45 #x67)
```
