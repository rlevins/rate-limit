;;;; rate-limit.lisp
(uiop/package:define-package #:rate-limit
    (:use #:cl)
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
   #:rate-limit-pointer

   ;; add event to the RATE-LIMIT
   #:increment-event

   ;; Conditions
   #:rate-limit-exceeded

   ;; Available Restarts
   #:continue-exceed-rate-limit 
   #:retry-with-backoff))

(in-package #:rate-limit)

;;; "rate-limit" goes here. Hacks and glory await!
(defvar *version* "0.0.1"
  "Rate Limit Version")

(defparameter *increment-event-lock*
  (bt:make-lock  "rate-limit:increment-event-lock")
  "bordeaux-threads lock for increment event, not recursive")

(defparameter *muffle-warnings* nil
  "Use to muffle the warnings printed when invoking the
'retry-with-backoff' restart")

(defparameter *default-count* 3
  "Default value of COUNT for creating RATE-LIMIT objects")

(defparameter *default-count-multiplier* 2)

(defparameter *default-interval* 3
  "Default value of INTERVAL for creating RATE-LIMIT objects")

(defclass rate-limit ()
  ((events  :initarg :events
            :initform nil
            :accessor rate-limit-events
            :documentation "events seen, each one is a universal time stamp")
   (interval :initarg :interval
             :initform *default-interval* 
             :accessor rate-limit-interval
             :documentation "the interval in seconds for counting events")
   (count    :initarg :count
             :initform *default-count* 
             ::accessor rate-limit-count
             :documentation "The allowable count of events in the given interval")
   (last-count :initarg :last-count
               :initform 0
               :accessor rate-limit-last-count
               :documentation "the event count as of the last seen event")
   (pointer  :initform 0
             :accessor rate-limit-pointer
             :documentation "position of the last event")))

(defmethod print-object ((rate-limit rate-limit) stream)
  "Prints RATE-LIMIT  Count + Interval instead of ID"
  (print-unreadable-object (rate-limit stream :type t)
    (format stream "~S/~s ~d"
            (rate-limit-count rate-limit)
            (rate-limit-interval rate-limit)
            (current-rate rate-limit))))

(defun current-rate (rate-limiter)
  (/ (rate-limit-last-count rate-limiter)
     (rate-limit-interval rate-limiter)))

(defun make-rate-limit (&optional (count *default-count*) (interval *default-interval*))
  "Creates an CLOS RATE-LIMIT object based on a COUNT
over an INTEVERAL given in seconds
e.g.

   ;; API requests are limited to one call per user every three seconds
   (make-rate-limit 1 3)        ; #<RATE-LIMIT 1/3>
   
   ;; once every five minutes
   (make-rate-limit 1 (* 5 60)) ; #<RATE-LIMIT 1/300>

   ;; 10 per 1 min
   (make-rate-limit 10 60)      ; #<RATE-LIMIT 10/60>

"
  (make-instance 'rate-limit
                 :events (make-array (* *default-count-multiplier* count))
                 :count count
                 :interval interval
                 :last-count 0))

(define-condition rate-limit-exceeded (error) 
  ((last-count :initarg :count :reader last-count)
   (limit :initarg :limit :reader limit)
   (interval :initarg :interval :reader interval))
  (:report (lambda (condition stream)
             (format stream
                     "Internal Rate Limit Will Be Exceeded:
Current - COUNT of ~a over INTERVAL of ~a seconds exceeds
LIMIT   - COUNT of ~a over INTERVAL of ~a seconds "
                     (when (slot-boundp condition 'last-count)
                       (last-count condition))
                     (when (slot-boundp condition 'interval)
                       (interval condition))
                     (when (slot-boundp condition 'limit)
                       (limit condition))
                     (when (slot-boundp condition 'interval)
                       (interval condition))))))

(defun rate-limit-last-pointer (rate-limiter)
  (if (= (rate-limit-pointer rate-limiter) 0)
          (1- (* *default-count-multiplier* (rate-limit-count rate-limiter)))
          (1- (rate-limit-pointer rate-limiter))))

(defun increment-event (rate-limiter)
  "Adds an event to the RATE-LIMITER represented
at the current time.  If the new-count is greater than the count
signals 'RATE-LIMIT-EXCEEDED error
Returns no useful value
Available restarts:
     retry-with-backoff - calculates a backoff time and sleeps for that time before
                          retrying the INCREMENT-EVENT
     "
  (bt:with-lock-held (*increment-event-lock*)
    ;; store current-pointer
    (let ((current-pointer (rate-limit-pointer rate-limiter)))
      ;; add new event at pointer
      (setf (aref (rate-limit-events rate-limiter) current-pointer)
                  (get-universal-time))
      ;; increments pointer; wraps to 0 if over (* *DEFAULT-COUNT-MULTIPLIER* COUNT)
      (setf (rate-limit-pointer rate-limiter) (mod (1+ current-pointer)
                                                   (* *default-count-multiplier* (rate-limit-count rate-limiter))))
      ;; update last-count
      (setf (rate-limit-last-count rate-limiter)
          (let* ((start (aref  (rate-limit-events rate-limiter) current-pointer))
                 (stop (- start (rate-limit-interval rate-limiter)))
                 (new-count 0))
            (loop for event across (rate-limit-events rate-limiter)
                        do (if (>= event stop) (incf new-count)))
            new-count))))
  (restart-case 
      (when (> (rate-limit-last-count rate-limiter) 
               (rate-limit-count rate-limiter))
        (error 'rate-limit-exceeded
               :count (rate-limit-last-count rate-limiter)
               :interval (rate-limit-interval rate-limiter)
               :limit (rate-limit-count rate-limiter)))
    (retry-with-backoff (&optional c (duration (calc-backoff
                                                rate-limiter
                                                (get-universal-time))))
      :report (lambda (stream)
                (format stream "Check again after sleeping ~a seconds to not exceed the rate-limit"  (calc-backoff rate-limiter (get-universal-time))))
      (unless *muffle-warnings*
        (warn "invoking retry-with-backoff for ~a seconds due to: ~a"
              duration (or c "unknown error")))
      ;(pop (rate-limit-events rate-limiter))
      (setf (rate-limit-pointer rate-limiter)
            (rate-limit-last-pointer rate-limiter))
      (decf (rate-limit-last-count rate-limiter))
      (sleep duration)
      (increment-event rate-limiter))
    (continue-exceed-rate-limit (&optional c)
      :report (lambda (stream)
                (format stream "Proceed and Exceed the Rate Limit"))
      (unless *muffle-warnings*
        (if (> 0 (calc-backoff rate-limiter (get-universal-time)))
            (warn "Continuing. This action will Exceed Rate Limit in spite of: ~a" (or c "unknown error"))
            (warn "Continuing. This action did not Exceed Rate Limit")))
      (values)))
  (values))

(defun retry-with-backoff (c)
  "Invokes RATE-LIMIT:RETRY-WITH-BACKOFF"
  (invoke-restart 'retry-with-backoff c))

(defun calc-backoff (rate-limiter time)
  "calculates the backoff in seconds needed to avoid the
RATE-LIMIT-LIMIT at the given TIME"
  (with-slots (events interval count pointer)
      rate-limiter
    (let ((backoff (+ (aref events (rate-limit-last-pointer rate-limiter))   ;(nth (- count 1) events)
                      interval
                      1
                      (- time))))
      (if (>= backoff 0)
          backoff
          0))))

(defmacro with-rate-limit (count interval rate-limit-exceeded-fn
                           &body body
                           &aux (rate-limit (gensym "rate-limit")))
  `(let ((,rate-limit (make-rate-limit ,count ,interval)))
     (if (> (+ 1 (rate-limit-last-count ,rate-limit))
            ,count)
         (progn
           (increment-event)
           ,@body)
         (funcall ,rate-limit-exceeded-fn))))



(defmacro def-rate-limited-fun (symbol lambda-list &body body)
  "Defines a rate-lmited function named SYMBOL that is rate limited to  
a default of *default-count* per *default-interval*  SYMBOL can also be a list:

(FN-NAME &rest rate-limit-init-args &key COUNT INTERVAL)

* COUNT - the allowable count per the INTERVAL, defaults to *default-count*
* INTERVAL - The inteveral in seconds to count, defaults to *default-interval*

(def-rate-limited-fun symbol (lambda-list) ...)
(def-rate-limited-fun (symbol &key count interval) (lambda-list) ...)
EG:


"
  (destructuring-bind (fn-name
                       &rest rate-limit-args
                       &key (count *default-count*) (interval *default-interval*)
                       &allow-other-keys)
      (if (listp symbol)
	  symbol
	  (list symbol))
    (declare (ignore rate-limit-args))
    (let* ((rate-limit (gensym "rate-limit"))
           (doc (when (stringp (first body)) (first body))))
      `(progn
	 (let ((,rate-limit (make-rate-limit ,count ,interval)))
	  (defun ,fn-name ,lambda-list
	    ,doc
	    (increment-event ,rate-limit)
	    ,@body))))))

(defmacro with-retry (&body body)
  `(handler-bind ((rate-limit:rate-limit-exceeded
                   #'(lambda (c)
                       (unless rate-limit:*muffle-warnings*) 
                       (rate-limit:retry-with-backoff c))))
     ,@body))

