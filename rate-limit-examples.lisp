;; Examples from README ... must have loaded rate-limit

(uiop/package:define-package #:rate-limit-examples
  (:use #:cl #:rate-limit))

(in-package :rate-limit-examples)

(def-rate-limited-fun (print-time :count 2 :interval 5)
    (&optional (full-? t))
  "A random API call, limited to 2 calls per 5 seconds"
  (let ((day-names '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday")))      
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore dst-p))
    (if full-?
     (format t "~%*** It is now ~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)~%~%"
	     hour
	     minute
	     second
	     (nth day-of-week day-names)
	     month
	     date
	     year
	     (- tz))
     (format t "~%*** It is now ~2,'0d:~2,'0d:~2,'0d~%~%"
	     hour
	     minute
	     second)))))

(defun run-example ()
  "Loops the test function to demonstrate the 'with-retry' macro and the rate-limit features"
  (format t "Loops the test function to demonstrate the 'with-retry' macro and the rate-limit features")
  (format t "The TEST function is a funtion to print the current date and time ,but is rate limited to 2 calls per 5 seconds~%")
 (loop for i from 1 to 5 
    do (let ((duration 1))
         (warn "Sleeping ~a seconds" duration)
         (sleep duration)
         (with-retry (print-time )))))
