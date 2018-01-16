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



;; Copyright (c) 2013 Russ Tyndall , Acceleration.net
;; http://www.acceleration.net All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

