(asdf:defsystem :cl-binary-test
  :description "Unit test for cl-binary"
  :author "Hiroyuki Tanaka <tanakahx@gmail.com>"
  :license "MIT"
  :depends-on (:cl-binary :fiveam)
  :components ((:module "t"
                :components
                ((:file "cl-binary-test"))))
  :perform (test-op (o s) 
                    (dolist (suite '(:cl-binary-test))
                      (symbol-call :fiveam :run! (find-symbol* suite :cl-binary-test)))
                    (asdf:clear-system s)))
