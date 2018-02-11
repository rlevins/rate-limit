# RATE-LIMIT

A common lisp library that provides a rate-limit object, functions and macros, which will signal an error before a rate-limited function is called, and provides restarts.

See rate-limit-examples.lisp for quicklisp loadable examples.


## Installation 

The preferred way to install RATE-LIMIT is through [Quicklisp](http://www.quicklisp.org/): 

`git clone https://github.com/rlevins/rate-limit.git`


```lisp
(ql:quickload :rate-limit)
```

For functioning examples:
```lisp
(ql:quickload :rate-limit-examples)
```

To run the test suite:
For functioning examples:
```lisp
(ql:quickload :rate-limit/test)
(rate-limit/test:run-my-tests)
```


## API
The RATE-LIMIT package exports the following symbols:

```lisp
(:export
   ;; class & initialize function
   #:rate-limit
   #:make-rate-limit
   ;; Macros
   #:def-rate-limited-fun
   #:with-retry
   #:*muffle-warnings*

   ;; Accessor functions
   #:rate-limit-events
   #:rate-limit-interval
   #:rate-limit-count
   #:rate-limit-last-count

   ;; add event to the RATE-LIMIT
   #:increment-event

   ;; Conditions
   #:rate-limit-exceeded

   ;; Available Restarts
   #:continue-exceed-rate-limit 
   #:retry-with-backoff)
   ```
   
### RATE-LIMIT 
 CLOS class for RATE-LIMIT, keeps track of number of times INCREMENT-EVENT is called for each RATE-LIMIT object.

### MAKE-RATE-LIMIT 
`(&optional (count *default-count*) (interval *default-interval*))`

### INCREMENT-EVENT
`(RATE-LIMIT)`

### DEF-RATE-LIMITED-FUNCTION

Creates a rate-limited function named SYMBOL, SYMBOL can also be a list

`(FN-NAME &rest rate-limit-init-args &key COUNT INTERVAL)`
    * FN-NAME - the name of the function 
    * COUNT - the allowable count per the INTERVAL, defaults to *default-count*
    * INTERVAL - The inteveral in seconds to count, defaults to *default-interval*


```lisp
(def-rate-limited-fun symbol (lambda-list) ...)
(def-rate-limited-fun (symbol &key count interval) (lambda-list) ...)

EG:

(def-rate-limited-fun (print-time :count 2 :interval 5)
    (&optional (full-? t))
  "A random API call, limited to 1 call per 60 seconds"
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
 (loop for i from 1 to 5 
    do (sleep (random 3))
      (with-retry (print-time ))))

(run-example)
;*** It is now 18:02:16 of Monday, 1/15/2018 (GMT-6)


;*** It is now 18:02:17 of Monday, 1/15/2018 (GMT-6)

; Warning: Invoking restart:
; While executing: (:INTERNAL RUN-EXAMPLE), in process repl-thread(14).
; Warning: invoking backoff for 4 seconds due to: Internal Rate Limit Will Be Exceeded:
;          Current - COUNT of 3 over INTERVAL of 5 seconds exceeds
;          LIMIT   - COUNT of 2 over INTERVAL of 5 seconds 
; While executing: INCREMENT-EVENT, in process repl-thread(14).

;*** It is now 18:02:23 of Monday, 1/15/2018 (GMT-6)


;*** It is now 18:02:25 of Monday, 1/15/2018 (GMT-6)

; Warning: Invoking restart:
; While executing: (:INTERNAL RUN-EXAMPLE), in process repl-thread(14).
; Warning: invoking backoff for 4 seconds due to: Internal Rate Limit Will Be Exceeded:
;          Current - COUNT of 3 over INTERVAL of 5 seconds exceeds
;          LIMIT   - COUNT of 2 over INTERVAL of 5 seconds 
; While executing: INCREMENT-EVENT, in process repl-thread(14).

;*** It is now 18:02:31 of Monday, 1/15/2018 (GMT-6)




```

# Author

```
;; Copyright (c) 2018 Rathan Levins 
;; All rights reserved.
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
```

