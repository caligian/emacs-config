(setq kill-buffer-query-functions nil)
(setq terminals (ht))
(setq terminal-shell nil)
(setq terminal-shell-command "/usr/bin/zsh")

(defclass terminal ()
  ((src :initarg :src
	:documentation "source src for the terminal")
   (process :initarg :process
	    :initform nil
	    :documentation "underlying terminal process")
   (command :initarg :command
	    :initform nil
	    :documentation "terminal command")
   (buffer :initarg :buffer
	   :initform nil
	   :documentation "terminal buffer")
   (type :initarg :type
	 :initform :buffer
	 :documentation "terminal type")
   (mode :init-arg :mode
	 :initform nil
	 :documentation "associated major mode")
   (lang :initarg :lang
	 :initform nil
	 :documentation "associated lang object")))

(defclass terminal-shell ()
  ((process :initarg :process
	    :initform nil
	    :documentation "underlying terminal process")
   (command :initarg :command
	    :initform nil
	    :documentation "terminal command")
   (buffer :initarg :buffer
	   :initform nil
	   :documentation "terminal buffer")))

(setq terminal-shell (terminal-shell :command terminal-shell-command))

(cl-defmethod terminal-find-paths ((obj terminal))
  (with-slots (src type) obj
    (let* ((mm (buffer-major-mode src)))
      (pcase type
	(:buffer
	 (fset% terminals mm (buffer-file-name src) obj))
	(:cwd
	 (fset% terminals mm (dirname src) obj))
	(:workspace
	 (if-let* ((workspace (find-buffer-workspace src)))
	     (fset% terminals mm workspace obj)))))))

(cl-defmethod terminal-get-major-mode ((obj terminal))
  (with-slots (src mode) obj
    (if mode
	mode
      (with-current-buffer src
	(setf (slot-value obj 'mode) major-mode)
	major-mode))))

(defun terminal-live? (obj)
  (with-slots (process) obj
    (when process
	(process-live-p process))))

(defun terminal-start (obj &optional split)
  (if (not (terminal-live? obj))
   (with-slots (command) obj
    (with-current-buffer (current-buffer)
      (pcase (or split :below)
	(:below (split-window-below))
	(:right (split-window-right)))
      (other-window 1)
      (ansi-term terminal-shell-command)
      (message "running REPL with command '%s'" command)
      (let* ((termbuf (current-buffer))
	     (process (get-buffer-process termbuf)))
	(setf (slot-value obj 'buffer) termbuf)
	(setf (slot-value obj 'process) process)
	(process-send-string process (concat command "\n"))
	obj)))
   (message "REPL with command '%s' is already running" (slot-value obj 'command))))

(defun terminal-split (obj &optional direction)
  (if (not (terminal-live? obj))
      (terminal-start obj direction)
    (when (and (terminal-live? obj)
	       (not (terminal-visible? obj)))
      (with-current-buffer (current-buffer)
	(pcase (or direction :below)
	  (:below (split-window-below))
	  (:right (split-window-right)))
	(with-slots (buffer) obj
	  (other-window 1)
	  (switch-to-buffer buffer))))))

(defun terminal-stop (obj)
  (with-slots (process) obj
    (when (terminal-live? obj)
      (terminal-hide obj)
      (kill-process process))))

(defun terminal-kill (obj)
  (terminal-stop obj))

(defun terminal-visible? (obj)
  (with-slots (buffer) obj
    (get-buffer-window buffer)))

(defun terminal-hide (obj)
  (if-let* ((window (get-buffer-window (slot-value obj 'buffer))))
      (progn
	(delete-window window))))

(cl-defmethod terminal-get-lang ((obj terminal))
  (with-slots (src) obj
    (lang-get-buffer-lang src)))

(cl-defmethod terminal-get-command ((obj terminal))
  (with-slots (lang type src)  obj
    (lang-buffer-command src lang 'repl type)))

(cl-defmethod terminal-init ((obj terminal))
  (if-let* ((filetype (terminal-get-lang obj))
	    (T (intern (substr (symbol-name (slot-value obj 'type)) 1)))
	    (command (lang-buffer-command (%. obj 'src) filetype 'repl T)))
      (progn
	(setf (slot-value obj 'command) command)
	(setf (slot-value obj 'lang) filetype)
	(setf (slot-value obj 'mode) (terminal-get-major-mode obj))
	(terminal-find-paths obj)
	obj)))

(defun terminal-shell-send-region ()
  (interactive)
  (when (terminal-shell-live?)
    (with-slots (process) terminal-shell
	(with-current-buffer (current-buffer)
	  (process-send-string
	   process
	   (concat (buffer-substring-no-properties
		    (mark)
		    (point))
		   "\n"))))))

(defun terminal-shell-live? ()
  (terminal-live? terminal-shell))

(defun terminal-shell-send-line ()
  (interactive)
  (when (terminal-shell-live?)
    (with-slots (process) terminal-shell
	(with-current-buffer (current-buffer)
	  (process-send-string
	   process
	   (concat (buffer-substring-no-properties (line-beginning-position) (line-end-position)) "\n"))))))

(defun terminal-shell-send-buffer ()
  (interactive)
  (when (terminal-shell-live?)
    (with-slots (process) terminal-shell
	(with-current-buffer (current-buffer)
	  (process-send-string
	   process
	   (concat (buffer-substring-no-properties (point-min) (point-max)) "\n"))))))

(defun terminal-send-string (obj &rest strings)
  (when (terminal-live? obj)
    (with-slots (process) obj
      (process-send-string
       process
       (concat (string-join strings "\n") "\n")))))

(cl-defmethod terminal-send-region ((obj terminal))
  (when (terminal-live? obj)
    (with-slots (src process) obj
      (with-current-buffer src
	(when (mark)
	  (process-send-string
	   process
	   (concat (buffer-substring-no-properties (mark) (point)) "\n")))))))

(cl-defmethod terminal-send-line ((obj terminal))
  (when (terminal-live? obj)
    (with-slots (src process) obj
      (with-current-buffer src
	(process-send-string
	 process
	 (concat (buffer-substring-no-properties
		  (line-beginning-position)
		  (line-end-position))
		 "\n"))))))

(cl-defmethod terminal-send-buffer ((obj terminal))
  (when (terminal-live? obj)
    (with-slots (src process) obj
	(with-current-buffer src
	  (process-send-string
	   process
	   (concat (buffer-substring-no-properties
		    (point-min)
		    (point-max))
		   "\n"))))))

(defun terminal-shell-start (&optional direction)
  (interactive)
  (if (not (terminal-live? terminal-shell))
      (terminal-start terminal-shell direction))
  (message "shell is already running. Use SPC-xs or SPC-xv to split window"))


(defun terminal-shell-kill ()
  (interactive)
  (if (terminal-live? terminal-shell)
      (progn
	(terminal-shell-hide)
	(with-slots (process buffer) terminal-shell
	  (kill-process process)
	  (with-current-buffer buffer
	    (set-buffer-modified-p nil)
	    (kill-current-buffer)
	    (setf (slot-value terminal-shell 'process) nil)
	    (setf (slot-value terminal-shell 'buffer) nil))))
    (message "REPL for shell is not running. SPC-xx to restart shell REPL")))

(defun terminal-shell-split (&optional direction)
  (interactive)
  (if (not (terminal-live? terminal-shell))
      (terminal-shell-start direction)
    (when (not (terminal-visible? terminal-shell))
      (terminal-split terminal-shell direction))))

(defun terminal-shell-hide ()
  (interactive)
  (when (terminal-visible? terminal-shell)
    (terminal-hide terminal-shell)))

(defun terminal-buffer-init (&optional buf)
  (let* ((buf (or buf (current-buffer)))
	 (bufname (buffer-file-name buf))
	 (workspace? (find-buffer-workspace buf))
	 (cwd? (dirname buf))
	 (mm (buffer-major-mode buf))
	 (buffer-term-exists? (get% terminals mm bufname))
	 (workspace-term-exists? (when workspace?
				   (get% terminals mm workspace?)))
	 (cwd-term-exists? (when cwd?
			     (get% terminals mm cwd?)))
	 (buffer-term (if (not buffer-term-exists?)
			  (terminal-init (terminal :src buf))
			buffer-term-exists?))
	 (workspace-term (if (not workspace-term-exists?)
			     (terminal-init (terminal :src buf :type :workspace))
			  workspace-term-exists?))
	 (cwd-term (if (not cwd-term-exists?)
		       (terminal-init (terminal :src buf :type :cwd))
		     workspace-term-exists?)))
    (ht (:buffer buffer-term)
	(:workspace workspace-term)
	(:cwd cwd-term))))

(defun terminal-buffer! (&optional buf type fn)
  (let* ((terms (terminal-buffer-init buf))
	 (term (get% terms (or type :buffer))))
    (funcall fn term)))

(defun terminal-buffer-split (&optional buf type split)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-split term split))))

(defun terminal-buffer-below (&optional buf type)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-split term :below))))

(defun terminal-buffer-right (&optional buf type)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-split term :right))))

(defun terminal-buffer-start-below (&optional buf type)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-start term :below))))

(defun terminal-buffer-start-right (&optional buf type)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-start term :right))))

(defun terminal-buffer-hide (&optional buf type)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-hide term))))

(defun terminal-buffer-kill (&optional buf type)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-kill term))))

(defun terminal-buffer-start (&optional buf type split)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-start term split))))

(defun terminal-buffer-send-string (&optional buf type s)
  (terminal-buffer! buf type (lambda (term) (terminal-send-string term s))))

(defun terminal-buffer-send-region (&optional buf type)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-send-region term))))

(defun terminal-buffer-send-line (&optional buf type)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-send-line term))))

(defun terminal-buffer-send-buffer (&optional buf type)
  (interactive)
  (terminal-buffer! buf type (lambda (term) (terminal-send-buffer term))))

(defun terminal-cwd-hide (&optional buf)
  (terminal-buffer! buf :cwd (lambda (term) (terminal-hide term))))

(defun terminal-cwd-kill (&optional buf)
  (terminal-buffer! buf :cwd (lambda (term) (terminal-kill term))))

(defun terminal-cwd-below (&optional buf)
  (terminal-buffer! buf :cwd (lambda (term) (terminal-split term :below))))

(defun terminal-cwd-right (&optional buf)
  (terminal-buffer! buf :cwd (lambda (term) (terminal-split term :right))))

(defun terminal-cwd-start-below (&optional buf)
  (interactive)
  (terminal-buffer! buf :cwd (lambda (term) (terminal-start term :below))))

(defun terminal-cwd-start-right (&optional buf split)
  (interactive)
  (terminal-buffer! buf :cwd (lambda (term) (terminal-start term :right))))

(defun terminal-cwd-send-region (&optional buf)
  (interactive)
  (terminal-buffer! buf :cwd (lambda (term) (terminal-send-region term))))

(defun terminal-cwd-send-line (&optional buf)
  (interactive)
  (terminal-buffer! buf :cwd (lambda (term) (terminal-send-line term))))

(defun terminal-cwd-send-buffer (&optional buf)
  (interactive)
  (terminal-buffer! buf :cwd (lambda (term) (terminal-send-buffer term))))

(defun terminal-workspace-hide (&optional buf)
  (interactive)
  (terminal-buffer! buf :workspace (lambda (term) (terminal-hide term))))

(defun terminal-workspace-kill (&optional buf)
  (interactive)
  (terminal-buffer! buf :workspace (lambda (term) (terminal-kill term))))

(defun terminal-workspace-below (&optional buf)
  (interactive)
  (terminal-buffer! buf :workspace (lambda (term) (terminal-split term :below))))

(defun terminal-workspace-right (&optional buf)
  (interactive)
  (terminal-buffer! buf :workspace (lambda (term) (terminal-split term :right))))

(defun terminal-workspace-start-below (&optional buf split)
  (interactive)
  (terminal-buffer! buf :workspace (lambda (term) (terminal-start term :below))))

(defun terminal-workspace-start-right (&optional buf split)
  (interactive)
  (terminal-buffer! buf :workspace (lambda (term) (terminal-start term :right))))

(defun terminal-workspace-send-region (&optional buf)
  (interactive)
  (terminal-buffer! buf :workspace (lambda (term) (terminal-send-region term))))

(defun terminal-workspace-send-line (&optional buf)
  (interactive)
  (terminal-buffer! buf :workspace (lambda (term) (terminal-send-line term))))

(defun terminal-workspace-send-buffer (&optional buf)
  (interactive)
  (terminal-buffer! buf :workspace (lambda (term) (terminal-send-buffer term))))
