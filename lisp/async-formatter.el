;; (setq formatter-buffer-processes (ht))
;; (setq formatter-region-processes (ht))

(setq buffer-formatters (ht))

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
  (when-let* ((obj (apply #'formatter args))
	      (buf (%. obj 'buffer))
	      (mm (buffer-major-mode buf))
	      (command (or (%. obj 'command)
			   (%. langs mm 'formatter))))
    (%! obj 'command command)
    (%! obj 'major-mode mm)
    (%! buffer-formatters buf obj)
    obj))

(defalias 'formatter! 'formatter-init)

(cl-defmethod formatter--create-on-success ((obj formatter) type)
  `(lambda (P)
     (with-slots (stdout stdin-file) P
       (with-current-buffer (%. ,obj 'buffer)
	 (save-buffer)
	 (when (eq ,type :buffer)
	   (beginning-of-buffer)
	   (set-mark (point))
	   (end-of-buffer))
	 (delete-region (mark) (point))
	 (insert (string-join stdout "\n"))
	 (deactivate-mark))
       (when (and (string? stdin-file)
		  (file-exists-p stdin-file)
		  (=~ stdin-file "^async-formatter-"))
	 (delete-file stdin-file)
	 (kill-buffer (get-buffer stdin-file))))))

(cl-defmethod formatter--create-on-failure ((obj formatter))
  `(lambda (P)
     (let* ((buf (get-buffer-create (make-temp-name "async-formatter-stderr-"))))
       (with-current-buffer (current-buffer)
	 (split-window-vertically)
	 (other-window 1)
	 (switch-to-buffer buf)
	 (dolist (l (%. P 'stderr))
	   (insert l))
	 (kbd! :keymaps 'local :states '(normal insert) "q" 'kill-buffer-and-window)))
     (message "could not format %s"
	      (buffer-file-name (%. ,obj 'buffer))
	      (%. P 'command))))

(cl-defmethod formatter-run ((obj formatter) type)
  "Run a formatter processs and overwrite the area in question which
is defined by `type' which can be either :buffer | :region"
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
    (%! buffer-formatters (%. obj 'buffer) obj)
    obj))

(defun format-buffer! (&optional buf command type)
  (interactive)
  (let* ((buf (or buf (current-buffer)))
	 (type (or type :buffer))
	 (buf-fmt (%. buffer-formatters buf)))
    (if buf-fmt
	(formatter-run buf-fmt type)
      (when-let* ((fmt (if command
			   (formatter! :buffer buf :command command)
			 (formatter! :buffer buf))))
	(formatter-run fmt type)))))

(defun format-region! (&optional buf command)
  (interactive)
  (format-buffer! buf command :region))
