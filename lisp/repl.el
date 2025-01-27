(setq kill-buffer-query-functions nil)
(setq repl-mode-map (make-sparse-keymap))
(evil-set-initial-state 'term-mode 'emacs)

(defgroup repl ()
  "Convenient global terminal"
  :prefix "repl-")

(define-minor-mode repl-mode
  "Toggle repl-mode"
  :init-value nil
  :lighter (" REPL")
  :keymap repl-mode-map
  :group 'repl)

(class repl
  process process-buffer
  (shell nil)
  cwd
  (shell-command (local-config-get :shell-command))
  command
  use-input-file
  mode
  (input-filter
   (lambda (s it) s))
  help)

;; repl
;; (<path> (shell <repl>) (workspace <repl>))

(local-config-set
 :repls 'shell
 (repl :cwd "/home/skeletor"
	   :command (local-config-get :shell-command)))

(cl-defmethod repl--register ((it repl))
  (with-slots (shell cwd mode) it
	(if shell
		(local-config-set :repls mode cwd 'shell it)
	  (local-config-set :repls mode cwd 'workspace it))))

(cl-defmethod repl-init ((it repl) &optional buf)
  (let* ((buf (or buf (current-buffer))))
	(with-slots (cwd mode command input-filter use-input-file help) it
	  (when-let* ((mode (if (not mode)
							(buffer-major-mode buf)
						  mode))
				  (command (or command (local-config-get :modes mode 'repl 'command)))
				  (input-filter (or input-filter (local-config-get :modes mode 'repl 'input-filter)))
				  (use-input-file (or use-input-file (local-config-get :modes mode 'repl 'use-input-file)))
				  (help (or help (local-config-get :modes mode 'repl 'help)))
				  (command (when command (car command)))
				  (input-filter (when input-filter (car input-filter)))
				  (use-input-file (when use-input-file (car use-input-file)))
				  (help (when help (car help))))
		(when (not command)
		  (error "No command specified for %s" buf))
		(%setq it
			   'mode mode
			   'command command
			   'input-filter input-filter
			   'use-input-file use-input-file
			   'help help
			   'cwd (or (find-buffer-workspace buf) (dirname buf)))
		(repl--register it)
		it))))

(cl-defun repl-buffer-get-repl (&optional buf (type 'shell))
  (let* ((buf (or buf (current-buffer))))
	(local-config-get
	 :repls
	 (buffer-major-mode buf)
	 (or (find-buffer-workspace buf) (dirname buf))
	 type)))

(cl-defmethod repl-live? ((it repl))
  (process-live-p (%. it 'process)))

(cl-defmethod repl-shell? ((it repl))
  (eq (%. it 'shell) t))

(cl-defmethod repl-start ((it repl))
  (if (repl-live? it)
	  it
	(with-slots (command cwd shell shell-command) it
	  (split-window-vertically)
	  (other-window 1)
	  (let* ((proc-buf (ansi-term shell-command))
			 (proc (get-buffer-process proc-buf)))
		(%setq it
			   'process proc
			   'process-buffer proc-buf)
		(process-send-string proc (concat "cd " cwd "\n"))
		(when (and command (not shell))
		  (process-send-string proc (concat command "\n")))
		(delete-window)
		it))))

(cl-defmethod repl-split ((it repl) &optional direction)
  (when (and (repl-live? it) (not (repl-visible? it)))
	(with-current-buffer (current-buffer)
	  (pcase (or direction :below)
		(:below (split-window-below))
		(:right (split-window-right)))
	  (with-slots (process-buffer) it
		(other-window 1)
		(switch-to-buffer process-buffer)))))

(cl-defmethod repl-split-right ((it repl))
  (repl-split it :right))

(cl-defmethod repl-stop ((it repl))
  (with-slots (process process-buffer) it
    (when (repl-live? it)
      (repl-hide it)
      (kill-process process)
      (kill-buffer process-buffer))))

(cl-defmethod repl-kill ((it repl))
  (repl-stop it))

(cl-defmethod repl-visible? ((it repl))
  (with-slots (process-buffer) it
    (get-buffer-window process-buffer)))

(cl-defmethod repl-hide ((it repl))
  (if-let* ((window (get-buffer-window (%. it 'process-buffer))))
      (delete-window window)))

(defmacro repl-delete-input-file (path &optional time)
  `(run-at-time (or ,time "5 sec") nil (progn-lambda (f-delete ,path))))

(cl-defmethod repl-send-string ((it repl) s)
  (when (repl-live? it)
	(with-slots (input-filter use-input-file) it
      (let* ((s (if (string? s) s (string-join s "\n")))
			 (s (funcall input-filter s it)))
		(if use-input-file
			(let* ((tempfile (when use-input-file (make-temp-file "repl-input-")))
				   (tempfile-s (concat (format use-input-file tempfile) "\r\n")))
			  (f-write s 'utf-8 tempfile)
			  (process-send-string (%. it 'process) tempfile-s)
			  (repl-delete-input-file tempfile))
		  (process-send-string (%. it 'process) (concat s "\r")))))))

(cl-defmethod repl-send-region ((it repl) &optional buf)
  (when (repl-live? it)
	(with-current-buffer (or buf (current-buffer))
	  (when (mark)
		(repl-send-string 
		 it
		 (concat (buffer-substring-no-properties (mark) (point)) "\n"))))))

(cl-defmethod repl-send-line ((it repl) &optional buf)
  (when (repl-live? it)
	(with-current-buffer (or buf (current-buffer))
	  (repl-send-string
	   it
	   (concat (buffer-substring-no-properties
				(line-beginning-position)
				(line-end-position))
			   "\n")))))

(cl-defmethod repl-send-buffer ((it repl) &optional buf)
  (when (repl-live? it)
	(with-current-buffer (or buf (current-buffer))
	  (repl-send-string
	   it
	   (concat (buffer-substring-no-properties
				(point-min)
				(point-max))
			   "\n")))))

(defun repl--create-shell-functions ()
  (dolist (name '("start" "stop" "hide" "stop" "split" "split-right"))
    (eval (append@ (list 'defun (intern (concat "repl-shell-" name)) nil '(interactive))
				   (list (intern (concat "repl-" name)) (local-config-get :repls 'shell)))))
  (dolist (name '("send-region" "send-line" "send-buffer"))
	(eval (append@ `(defun ,(intern (concat "repl-shell-" name)) (&optional buf)
					  (interactive))
				   `(,(intern (concat "repl-" name)) (local-config-get :repls 'shell) buf)))))

(defun repl--create-mapping-functions ()
  (dolist (name '("start" "stop" "hide" "split" "split-right" "send-buffer" "send-line" "send-region"))
	(let* ((actual-fn (intern (concat "repl-" name))) 
		   (fn-name (intern (concat "repl-mapping-" name)))
		   (fn-root-name (intern (concat "repl-mapping-root-" name))))
	  (eval `(defun ,fn-root-name (&optional buf)
			   (interactive)
			   (let* ((buf (or buf (current-buffer)))
					  (it (repl-buffer-get-repl buf 'shell))
					  (it (or it (repl-init (repl :shell t) buf))))
				 (,actual-fn it))))
	  (eval `(defun ,fn-name (&optional buf)
			   (interactive)
			   (let* ((buf (or buf (current-buffer)))
					  (it (repl-buffer-get-repl buf 'workspace))
					  (it (or it (repl-init (repl) buf))))
				 (,actual-fn it)))))))

(defun repl--create-mappings ()
  (define-key! repl (:prefix "SPC" :keymaps 'repl-mode-map)
	(normal
	 "<return><return>" 'repl-mapping-root-start
	 "<return>s" 'repl-mapping-root-split
	 "<return>v" 'repl-mapping-root-split-right
	 "<return>e" 'repl-mapping-root-send-line
	 "<return>b" 'repl-mapping-root-send-buffer
	 "<return>k" 'repl-mapping-root-hide
	 "<return>q" 'repl-mapping-root-stop)
	(normal
	 "rr" 'repl-mapping-start
	 "rs" 'repl-mapping-split
	 "rv" 'repl-mapping-split-right
	 "re" 'repl-mapping-send-line
	 "rb" 'repl-mapping-send-buffer
	 "rk" 'repl-mapping-hide
	 "rq" 'repl-mapping-stop)
	(visual
	 "re" 'repl-mapping-send-region
	 "<return>e" 'repl-mapping-root-send-region))

  (define-key! repl-shell (:prefix "SPC x")
	(normal
	 "x" 'repl-shell-start
	 "s" 'repl-shell-split
	 "v" 'repl-shell-split-right
	 "e" 'repl-shell-send-line
	 "b" 'repl-shell-send-buffer
	 "k" 'repl-shell-hide
	 "q" 'repl-shell-stop)
	(visual
	 "e" 'repl-shell-send-region)))

(repl--create-shell-functions)
(repl--create-mapping-functions)
(repl--create-mappings)
