;; (setq formatter-buffer-processes (ht))
;; (setq formatter-region-processes (ht))

(defclass formatter ()
  ((major-mode
    :initarg :major-mode
    :initform nil)
   (command
    :initarg :command
    :initform nil)
   (buffer
    :initarg :buffer
    :initform nil)
   (last-process
    :initarg :last-process
    :initform nil)
   (exit-status
    :initarg :exit-status
    :initform nil)))

(defun formatter-init (&rest args)
  (let* ((obj (apply #'formatter args))
	 (buf (%. obj 'buffer))
	 (mm (buffer-major-mode buf))
	 (command (or (%. obj 'command)
		      (%. langs mm 'formatter))))
    (%! obj 'command command)
    (%! obj 'major-mode mm)
    obj))

(defalias 'formatter! 'formatter-init)

(cl-defmethod formatter--create-on-success ((obj formatter) type)
  `(lambda (P)
     (with-slots (stdout stdin-file) P
       (with-current-buffer (%. ,obj 'buffer)
	 (when (eq ,type :buffer)
	   (beginning-of-buffer)
	   (set-mark (point))
	   (end-of-buffer))
	 (delete-region (mark) (point))
	 (insert (string-join stdout "\n"))
	 (deactivate-mark))
       (when (and (string? stdin-file) (file-exists-p stdin-file))
	 (delete-file stdin-file)))))

(cl-defmethod formatter--create-on-failure ((obj formatter))
  `(lambda (P)
     (message "could not format %s"
	      (buffer-file-name (%. ,obj 'buffer))
	      (%. P 'command))))

(cl-defmethod formatter-run ((obj formatter) type)
  "Run a formatter processs and overwrite the area in question which
is defined by `type' which can be either 'buffer | 'region"
  (when-let* ((connected (%. obj 'buffer))
	      (type (or type :buffer))
	      (s (if (eq type :buffer)
		     (buffer2string connected)
		   (buffer-get-region connected)))
	      (command (%. obj 'command))
	      (on-success (eval (formatter--create-on-success obj type)))
	      (on-failure (eval (formatter--create-on-failure obj)))
	      (proc (async-process!
		     :name (make-temp-name "async-formatter-")
		     :command command
		     :with-stdin s
		     :on-success on-success
		     :on-failure on-failure))
	      (proc (async-process-start proc)))
    (%! obj 'last-process proc)
    obj))


(setq a-formatter (formatter! :buffer (current-buffer) :command "cat"))
(formatter-run a-formatter :region)
