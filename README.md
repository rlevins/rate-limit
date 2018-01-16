# rate-limit

A common lisp library that provides a rate-limit object and function, which will signal an error before a rate-limited function is called. 


## RATE-LIMIT


## API

### DEF-RATE-LIMITED-FUNCTION

Creates a rate-limited function named SYMBOL, SYMBOL can also be a list

`(FN-NAME &rest rate-limit-init-args &key COUNT INTERVAL)`

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


(with-retry (print-time))  ; *** It is now 17:51:46 of Monday, 1/15/2018 (GMT-6)
(with-retry (print-time))  ; *** It is now 17:51:47 of Monday, 1/15/2018 (GMT-6)
(with-retry (print-time))
; Warning: Invoking restart:
; While executing: #<Anonymous Function #x3020024068DF>, in process repl-thread(14).
; Warning: invoking backoff for 5 seconds due to: Internal Rate Limit Will Be Exceeded:
;          Current - COUNT of 3 over INTERVAL of 5 seconds exceeds
;          LIMIT   - COUNT of 2 over INTERVAL of 5 seconds 
; While executing: INCREMENT-EVENT, in process repl-thread(14).

;*** It is now 17:51:53 of Monday, 1/15/2018 (GMT-6)

(with-retry (print-time)) ; *** It is now 17:51:55 of Monday, 1/15/2018 (GMT-6)
NIL
```


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

