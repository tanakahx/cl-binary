(asdf:defsystem :cl-binary
  :description "Binary stream IO library"
  :author "Hiroyuki Tanaka <tanakahx@gmail.com>"
  :license "MIT"
  :depends-on (:trivial-gray-streams)
  :components ((:module "src"
                :components
                ((:file "cl-binary" :depends-on ("io"))
                 (:file "io"))))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-binary-test))))
