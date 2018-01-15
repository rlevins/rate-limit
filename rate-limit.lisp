;;;; rate-limit.lisp
(uiop/package:define-package #:rate-limit
  (:use #:cl)
  (:export
   #:rate-limit
   #:make-rate-limit

   #:rate-limit-events
   #:rate-limit-interval
   #:rate-limit-count
   #:rate-limit-last-count
   #:increment-event

   #:rate-limit-exceeded
   #:retry-with-backoff))

(in-package #:rate-limit)

;;; "rate-limit" goes here. Hacks and glory await!
(defvar *version* "0.0.1")

(defparameter *increment-event-lock*
  (bt:make-lock  "rate-limit:increment-event-lock"))

(defparameter *muffle-warnings* nil)

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
		     "Internal Rate Limit Exceeded
COUNT of ~a over INTERVAL ~a seconds
Exceeds LIMIT of COUNT ~a over INTERVAL of ~a seconds "
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
  (let ((error-? nil))
    (bt:with-lock-held (*increment-event-lock*)
      (push (get-universal-time)
	    (rate-limit-events rate-limit))
      (when (> (update-last-count rate-limit)
	       (rate-limit-count rate-limit))
	(pop (rate-limit-events rate-limit))
	(decf (rate-limit-last-count rate-limit))
	(setf error-? t)))
    (if error-?
	(restart-case 
	    (error 'rate-limit-exceeded
		   :count (+ 1 (rate-limit-last-count rate-limit))
		   :interval (rate-limit-interval rate-limit)
		   :limit (rate-limit-count rate-limit))
	  (retry-with-backoff
	      (&optional c
		 (duration (calc-backoff rate-limit
					 (get-universal-time))))
	    (unless *muffle-warnings*
		(warn "invoking backoff due to: ~a" c))
	    (log:info duration)
	    (sleep duration)
	    (increment-event rate-limit)))))
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
		 (- time))))
      (if (>= backoff 0)
	  backoff
	  0))))

(declaim (inline update-last-count))
(defun update-last-count (rate-limit)
  "Updates the last-count of the RATE-LIMIT.  Not threadsafe."
  (with-slots (events interval last-count)
      rate-limit
    (let ((stop-time (- (car events) interval)))
      (setf last-count 0)
      (block walk-list-loop
	(dotimes (i (length events)) 
	  (if (> (nth i events) stop-time)
	      ;; count event if time in bounds
	      (incf last-count)
	      ;; truncate events if time exceeds interval and return
	      (progn
		(setf (nthcdr i events) nil)
		(return-from walk-list-loop)))))
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
