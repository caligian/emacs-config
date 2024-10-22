(setq user-async-processes (ht))

(defclass async-process ()
  ((name :initarg :name
	 :initform nil)
   (command :initarg :command
	 :initform nil)
   (buffer :initarg :buffer
	   :initform nil)
   (process :initarg :process
	    :initform nil)
   (exit-status :initarg :exit-status
		:initform nil)
   (stderr-pipe :initarg :stderr-pipe
		:initform nil)
   (stdout :initarg :stdout
	   :initform nil)
   (stderr :initarg :stderr
	   :initform nil)
   (filter :initarg :filter
	   :initform nil)
   (sentinel :initarg :sentinel
	     :initform nil)))

(cl-defmethod async-process--create-filter ((proc async-process))
  `(with-slots (process stdout) ,proc
     (lambda (_ s)
       (%! ,proc 'stdout (append stdout (split-string s "\n\r"))))))

(cl-defmethod async-process--create-sentinel ((proc async-process))
  `(with-slots (process) ,proc
     (lambda (_ e)
       (unless (process-live-p ,proc)
	(%! ,proc 'exit-status (process-exit-status process))))))

(cl-defmethod async-process--create-stderr-pipe ((proc async-process))
  `(with-slots (name process stderr) ,proc
     (make-pipe-process
      :name (concat name "-stderr-pipe")
      :filter (lambda (_ s)
		(%! ,proc 'stderr (append stderr (split-string s "\n\r")))))))

(defun async-process-init (&rest args)
  (let* ((proc (apply #'async-process args))
	 (name (%. proc 'name)))
    (when name
      (%! user-async-processes name proc))
    proc))

(cl-defmethod async-process-live? ((proc async-process))
  (with-slots (process) proc
    (and process (process-live-p process))))

(cl-defmethod async-process-start ((proc async-process))
  (if (async-process-live? proc)
      proc
    (with-slots (name command filter sentinel) proc
	(let* ((filter (eval (or filter (async-process--create-filter proc))))
	       (stderr-pipe (eval (async-process--create-stderr-pipe proc)))
	       (sentinel (eval (or sentinel (async-process--create-sentinel proc))))
	       (process (make-process
			 :connection-type 'pipe
			 :name name
			 :filter filter
			 :sentinel sentinel
			 :stderr stderr-pipe
			 :command command)))
	  (%! proc 'process process)
	  (%! user-async-processes name proc)
	  (%setq proc
		 'sentinel sentinel
		 'filter filter
		 'stderr-pipe stderr-pipe)
	  proc))))

(cl-defmethod async-process-send-string ((proc async-process) &rest strings)
  (when (async-process-live? proc)
    (cl-loop for s in strings do (process-send-string (%. proc 'process) s))))

(cl-defmethod async-process-send-buffer ((proc async-process) &optional buf)
  (let* ((buf (or buf (current-buffer)))
	 (region (buffer2string buf)))
    (async-process-send-string proc region)))

(cl-defmethod async-process-send-region ((proc async-process) &optional buf)
  (let* ((buf (or buf (current-buffer)))
	 (region (buffer-get-region buf)))
    (async-process-send-string proc region)))

;; (setq a-proc (async-process-init
;; 	      :name "test process"
;; 	      :command (list "ls" "-l1324324" "/home/skeletor")))

;; (async-process-start a-proc)
;; (%. a-proc 'exit-status)
;; (%. a-proc 'stderr)
;; a-proc
;; (process-exit-status (%. a-proc 'process))
