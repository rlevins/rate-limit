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
   #:increment-event
   ;; Error and restart function
   #:rate-limit-exceeded
   #:retry-with-backoff))

(in-package #:rate-limit)

;;; "rate-limit" goes here. Hacks and glory await!
(defvar *version* "0.0.1")

(defparameter *increment-event-lock*
  (bt:make-lock  "rate-limit:increment-event-lock"))

(defparameter *muffle-warnings* nil
  "Use to muffle the warnings printed when invoking the
'retry-with-backoff' restart")

(defparameter *default-count* 3)

(defparameter *default-interval* 3)

(defclass rate-limit ()
  ((events  :initarg :events
	     :initform nil
	     :accessor rate-limit-events)
   (interval :initarg :interval
	     :initform nil
	     :accessor rate-limit-interval)
   (count    :initarg :count
	     :initform 1
	     ::accessor rate-limit-count)
   (last-count :initarg :last-count
		 :initform 0
		 :accessor rate-limit-last-count)))

(defmethod print-object ((rate-limit rate-limit) stream)
  "Prints RATE-LIMIT  Count + Interval instead of ID"
  (print-unreadable-object (rate-limit stream :type t)
    (format stream "~S/~s"
	    (rate-limit-count rate-limit)
	    (rate-limit-interval rate-limit))))

(defun make-rate-limit (count interval)
  "Creates an CLOS RATE-LIMIT object based on a COUNT
over an INTEVERAL giving in seconds
e.g.
   API requests are limited to one call per user every three seconds
   (make-rate-limit 1 3)

   once every five minutes
   (make-rate-limit 1 (* 5 60))

   10 per 1 min
   (make-rate-limit 10 60)

"
  (make-instance 'rate-limit
		 :events nil
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

(defun increment-event (rate-limit)
  "Adds an event to the RATE-LIMIT represented
at the current time.  If the new-count is greater than the count
signals 'RATE-LIMIT-EXCEEDED error
Returns no useful value"
  (bt:with-lock-held (*increment-event-lock*)
    (push (get-universal-time)
	  (rate-limit-events rate-limit))
    (n-update-last-count rate-limit))
  (restart-case 
      (when (> (rate-limit-last-count rate-limit) 
	       (rate-limit-count rate-limit))
	(error 'rate-limit-exceeded
	       :count (rate-limit-last-count rate-limit)
	       :interval (rate-limit-interval rate-limit)
	       :limit (rate-limit-count rate-limit)))
    (retry-with-backoff (&optional c (duration (calc-backoff
						rate-limit
						(get-universal-time))))
      :report (lambda (stream)
		(format stream "Check again after sleeping ~a seconds to not exceed the rate-limit"  (calc-backoff rate-limit (get-universal-time))))
      (unless *muffle-warnings*
	(warn "invoking backoff for ~a seconds due to: ~a"
	      duration (or c "unknown error")))
      (pop (rate-limit-events rate-limit))
      (decf (rate-limit-last-count rate-limit))
      (sleep duration)
      (increment-event rate-limit))
    (continue-exceed-rate-limit (&optional c)
      :report (lambda (stream)
		(format stream "Proceed and Exceed the Rate Limit"))
      (unless *muffle-warnings*
	(if (> 0 (calc-backoff rate-limit (get-universal-time)))
	    (warn "Continuing. This action will Exceed Rate Limit in spite of: ~a" (or c "unknown error"))
	    (warn "Continuing. This action did not Exceed Rate Limit")))
      (values)))
  (values))

(defun retry-with-backoff (c)
  "Invokes RATE-LIMIT:RETRY-WITH-BACKOFF"
  (invoke-restart 'retry-with-backoff c))

(defun calc-backoff (rate-limit time)
  "calculates the backoff in seconds needed to avoid the
RATE-LIMIT-LIMIT at the given TIME"
  (with-slots (events interval count)
      rate-limit
    (let ((backoff (+ (nth (- count 1) events)
		      interval
		      1
		      (- time))))
      (if (>= backoff 0)
	  backoff
	  0))))

(declaim (inline n-update-last-count))
(defun n-update-last-count (rate-limit)
  "Updates the last-count of the RATE-LIMIT.  Not threadsafe."
  (with-slots (events interval last-count)
      rate-limit
    (let ((stop-time (- (car events) interval)))
      (setf last-count 0)
      (loop for el in events always (>= el stop-time)
	 do  (incf last-count))
      (setf (nthcdr last-count events) nil)
      (values last-count interval))))

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
                       &key (count *default-count*)
		       (interval *default-interval*)
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
		       (unless rate-limit:*muffle-warnings*
			 (warn "Invoking restart:"))
		       (rate-limit:retry-with-backoff c))))
     ,@body))
   
