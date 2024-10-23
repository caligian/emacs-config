(setq user-async-processes (ht))

(defclass async-process ()
  ((shell :initarg :shell
	  :initform t)
   (name :initarg :name
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
	     :initform nil)
   (with-stdin :initarg :with-stdin
	       :initform nil)
   (on-exit :initarg :on-exit
	       :initform nil)
   (on-failure :initarg :on-failure
	       :initform nil)
   (on-success :initarg :on-success
	       :initform nil)
   (stdin-file :initarg :stdin-file
	       :initform nil)))

(cl-defmethod async-process--create-shell-command ((proc async-process))
  (async-process--create-temp-file proc)
  (with-slots (command stdin-file) proc
    (let* (cmd)
      (if (listp command)
	  (setq cmd (string-join command " "))
	(setq cmd (%. proc 'command)))
      (if (and stdin-file (file-exists-p stdin-file))
	  (setq cmd (list "zsh" "-c" (concat "cat " stdin-file " | " cmd)))
	 (setq cmd (list "zsh" "-c" cmd)))
      (%! proc 'command cmd)
      cmd)))

(cl-defmethod async-process--create-temp-file ((proc async-process))
  (with-slots (with-stdin) proc
    (cond
     ((and (buffer? with-stdin) (buffer-file-name with-stdin))
      (%! proc 'stdin-file (buffer-file-name with-stdin)))
     ((string? with-stdin)
      (let* ((tempname (make-temp-name "async-process-stdin-"))
	     (tempname (concat config-directory "/.cache/" tempname))
	     (buf (get-buffer-create tempname)))
	(with-current-buffer buf
	  (local-set-key (kbd "q") #'delete-window)
	  (insert with-stdin)
	  (write-file tempname)
	  (%! proc 'stdin-file tempname)))))))

(cl-defmethod async-process--create-filter ((proc async-process))
  `(with-slots (process stdout) ,proc
     (lambda (_ s)
       (%! ,proc 'stdout (append stdout (split-string s "\n\r"))))))

(cl-defmethod async-process--create-sentinel ((proc async-process))
  `(with-slots (process on-exit on-success on-failure stdin-file) ,proc
     (lambda (_ e)
       (when-let* ((exit-status (process-exit-status process)))
	 (when (and (string? stdin-file)
		    (=~ stdin-file "async-process-stdin-"))
	   (delete-file stdin-file)
	   (when-let* ((buf (get-buffer stdin-file)))
	     (kill-buffer buf)))
	 (when on-exit
	   (funcall on-exit ,proc))
	 (let* ((success? (= exit-status 0)))
	   (when (and on-success success?)
	     (funcall on-success ,proc))
	   (when (and on-failure (not success?))
	     (funcall on-failure ,proc)))))))

(cl-defmethod async-process--create-stderr-pipe ((proc async-process))
  `(with-slots (name stderr) ,proc
     (make-pipe-process
      :name (concat name "-stderr-pipe")
      :sentinel (lambda (p _)
		  (when-let* ((exit-status (process-exit-status p)))
		    (when (buffer? (get-buffer name))
		      (kill-buffer (get-buffer name)))
		    (when (file-exists-p name)
		      (delete-file name))))
      :filter (lambda (_ s)
		(%! ,proc 'stderr (append stderr (split-string s "\n\r")))))))

(defun async-process-init (&rest args)
  (let* ((proc (apply #'async-process args))
	 (name (%. proc 'name)))
    (when name
      (%! user-async-processes name proc))
    (async-process--create-shell-command proc)
    proc))

(defalias 'async-process! 'async-process-init)

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

(cl-defmethod async-process-kill ((proc async-process))
  (when (async-process-live? proc)
    (kill-process proc)))

(cl-defmethod async-process-stop ((proc async-process))
  (when (async-process-live? proc)
    (stop-process proc)))

(setq a-proc
      (async-process-init
       :name "test process"
       :command (list "ls" "-l" "/home/skeletor" ">" "-some-output-file")
       :on-success (lambda (_) (shell-command "firefox"))))

