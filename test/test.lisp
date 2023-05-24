(defpackage #:rate-limit/test
  (:use #:cl #:fiveam
	#:rate-limit
	#:cl-ppcre
	#:local-time )
  (:export
   #:run-my-tests))


(in-package :rate-limit/test)

(defvar *system-under-test* (asdf:find-system "rate-limit" nil))

(defun create-test-scaffold ()
  (values))

(defun tear-down-scaffold ()
  (values))

(defun run-my-tests (&optional (save-report-to nil))
  "Runs tests, if SAVE-REPORT-TO provides a valid path
a text file will be produced, the file-name will be the
package-name-test-results-yyy-hh-mm-ss.txt"
  
  (let ((output-file (format nil "~a-test-results-~a-~a.txt"
			    (asdf:component-name *system-under-test*) 
			    (format-timestring nil (now)
					       :format +iso-8601-date-format+)
			    (format-timestring nil (now)
					       :format '(:hour "-" :min "-" :sec )))))
    (if save-report-to
	(let ((results
	       (with-output-to-string (out)
		 (let ((*standard-output* out))
		   (test-system)))))
	 (with-open-file (out-file
			  (merge-pathnames
			   ;;(user-homedir-pathname)
			   save-report-to
			   output-file)
			  :direction :output
					;:if-exists :append
			  :if-does-not-exist :create)
	   (format out-file "~a" results))
	 (format t "~a" results))
	(test-system))))

(defun test-system ()
  (create-test-scaffold)
  (log:config :warn)
  (format t "~%Tests ran: ~a~%" (now))
  (fiveam:run-all-tests)
  (log:config :info)
  (tear-down-scaffold))

(def-suite :rate-limit-all-tests)

(in-suite :rate-limit-all-tests)

(test t-make-rate-limit
  (is (eql 'rate-limit
	   (type-of (make-rate-limit 10 20)))))

(test t-rate-limit-exceeded-condition
  (signals rate-limit-exceeded
    (error 'rate-limit-exceeded
	   :interval 10
	   :count 10
	   :limit 9)))


(test (t-increment-event :depends-on (and t-make-rate-limit
					  t-rate-limit-exceeded-condition))
  (let ((rl (make-rate-limit 11 20)))
    (is (= 0 (rate-limit-last-count rl)))
    (increment-event rl)
    (is (= 1 (rate-limit-last-count rl)))
    (dotimes (i 10)
      (increment-event rl))
    (is (= 11 (rate-limit-last-count rl)))))

(test (t-increment-event-exceed-limit :depends-on (and t-increment-event))
  (let ((rl (make-rate-limit 10 20)))
    (signals rate-limit-exceeded
     (dotimes (i 12)
       (increment-event rl)))))


;; (test (t-increment-event-randomized
;;        :depends-on (and t-increment-event))
;;   (let ((rl (make-rate-limit 11 2)))
;;     (dotimes (i 11)
;;       (increment-event rl))
;;     (is (= 11 (rate-limit-last-count rl)))))

(test (t-calc-backoff :depends-on (and t-make-rate-limit))
  (let ((rate-limit (make-rate-limit 4 2)))
    (setf (rate-limit-events rate-limit)
          #(3724954000 3724954000 3724954000 3724954000 0 0 0 0 ))
    (setf (rate-limit-pointer rate-limit) 4)
    (is (= 3
	   (rate-limit::calc-backoff rate-limit
				     3724954000)))
    (is (/= 20
	   (rate-limit::calc-backoff rate-limit
				     3724954000)))))
(defun gen-events (&key num-events interval)
  (let ((list-gen
	 (gen-list
	  :length (gen-integer :min num-events :max num-events)
	  :elements (gen-integer :min (- (get-universal-time)
					 (* interval 2))
				 :max (get-universal-time)))))
    (values (make-array num-events :initial-contents (funcall list-gen))
	    (get-universal-time))))

(test (t-calc-backoff-0 :depends-on (and t-calc-backoff))
  (let ((rate-limit (make-rate-limit 5 9)))
    ;(setf (rate-limit-events rate-limit) '(3725012958 3725012958 3725012958 3725012958 3725012958))
    (setf (rate-limit-events rate-limit)
          #(3724954000 3724954000 3724954000 3724954000 0 0 0 0 ))
    (setf (rate-limit-pointer rate-limit) 4)
    (is (= 0
	   (rate-limit::calc-backoff rate-limit
				     3725012969))))
  (for-all ((count (gen-integer :min 1 :max 250))
	    (interval (gen-integer :min 1 :max 100)))
    "tests that the calc-backoff function always returns > 0 across boundaries"
    (let ((rate-limit (make-rate-limit count interval))
	  (events (gen-events :num-events (* 10 count) :interval interval )))
      (setf (rate-limit-events rate-limit) events)
      (is (every #'(lambda (x) (<= 0 x))
	      (loop for time from (aref events 0) to (+ (get-universal-time) (* 2 interval))
		 collecting  
		   (rate-limit::calc-backoff rate-limit time)))))))

(test (t-retry-with-backoff :depends-on (and t-calc-backoff-0))
  (is-true
   (let ((rate-limit (make-rate-limit 4 2)))
     (dotimes (i 10)
       (handler-bind
	   ((rate-limit-exceeded #'rate-limit::retry-with-backoff))
	 (increment-event rate-limit)))
     t)))
