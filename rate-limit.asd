;;;; rate-limit.asd

#-asdf3.1 (error "my-lib requires ASDF 3.1")

(asdf:defsystem #:rate-limit
  :description "Describe rate-limit here"
  :author "Your Name <your.name@example.com>"
  :license "BSD"
  :version (:read-file-form "rate-limit.lisp" :at (2 2))
  :class :package-inferred-system
  :serial t
  :depends-on (#:bordeaux-threads)
  :components ((:file "rate-limit"))
  :in-order-to ((test-op
		 (test-op "rate-limit/test"))))


(asdf:defsystem "rate-limit/test"
  :depends-on (#:rate-limit
	       #:fiveam
	       #:local-time
	       #:cl-fad
	       #:cl-ppcre
	       #:log4cl)
  :components ((:module "test"
			:serial t
			:components
			((:file "test"))))
  :perform
  (asdf:test-op (o s)
		(uiop:symbol-call "rate-limit/test"
				  '#:run-my-tests)))

(asdf:defsystem "rate-limit-examples"
  :depends-on (#:rate-limit)
  :components ((:file "rate-limit-examples")))


