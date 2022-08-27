(defsystem "cl-pyim"
  :version "0.1.0"
  :author "Rosario S.E."
  :license "LGPL-2"
  :depends-on ("mcclim"
	       "alexandria"
	       "bknr.datastore"
	       "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "test-frame"))))
  :description "in development"
  :in-order-to ((test-op (test-op "cl-pyim/tests"))))

(defsystem "cl-pyim/tests"
  :author "Rosario S.E."
  :license "LGPL-2"
  :depends-on ("cl-pyim"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
		 (:file "test-frame"))))
  :description "Test system for cl-pyim"
  :perform (test-op (op c) (symbol-call :rove :run c)))
