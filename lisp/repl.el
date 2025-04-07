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

(class repl ()
  process process-buffer
  command cwd
  mode
  (shell nil)
  (shell-command (cget :shell-command))
  use-input-file
  (input-filter
   (lambda (s it) s))
  help)

;; repl
;; (<path> (shell <repl>) (workspace <repl>))

(cset :repls 'shell
	  (repl :cwd (getenv "HOME")
			:mode 'shell
			:command (cget :shell-command)))

(cl-defmethod repl--register ((it repl))
  (with-slots (shell cwd mode) it
    (if shell
        (cset :repls mode cwd 'shell it)
      (cset :repls mode cwd 'workspace it))))

(cl-defun repl-init (&optional
					 buf
					 &key
					 command
					 cwd
					 shell
					 (shell-command (cget :shell-command))
					 use-input-file
					 (input-filter (lambda (s it) s))
					 help)
  (let* ((buf (or buf (current-buffer)))
		 (mode (buffer-major-mode buf))
		 (conf (when mode
				 (cget :modes mode)))
		 (conf (and conf (%. conf 'repl))))
	(when-let* ((command (if shell
							 shell-command
						   (if conf
							   (%. conf 'command)
							 command)))
				(command (if (listp command)
							 (string-join command " ")
						   command))
				(args (list :shell shell
							:mode mode
							:command command
							:cwd (or cwd
									 (find-buffer-workspace buf)
									 (dirname buf))))
				(input-filter (or (%. conf 'input-filter) -1))
				(use-input-file (or (%. conf 'use-input-file) -1))
				(args (if (not (number? use-input-file))
						  (append args (list :use-input-file use-input-file))
						args))
				(args (if (not (number? input-filter))
						  (append args (list :input-filter input-filter))
						args))
				(it (apply 'repl args)))
	  (repl--register it)
	  it)))

(cl-defun repl-buffer-get-repl (&optional buf (type 'shell))
  (let* ((buf (or buf (current-buffer))))
    (cget
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
    (with-slots (mode command cwd shell shell-command) it
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
		(if shell
			(message "Workspace terminal started for %s" cwd)
		  (if (eq mode 'shell)
			  (message "Terminal started for %s" cwd)
			(message "REPL (%s) started for %s" mode cwd)))
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
		 (buffer-substring-no-properties (mark) (point)))))))

(cl-defmethod repl-send-line ((it repl) &optional buf)
  (when (repl-live? it)
    (with-current-buffer (or buf (current-buffer))
      (repl-send-string
       it
	   (buffer-substring-no-properties
		(line-beginning-position)
		(line-end-position))))))

(cl-defmethod repl-send-buffer ((it repl) &optional buf)
  (when (repl-live? it)
    (with-current-buffer (or buf (current-buffer))
      (repl-send-string
       it
	   (buffer-substring-no-properties
		(point-min)
		(point-max))))))

(cl-defmethod repl-send-thing-at-point ((it repl) thing &optional buf)
  (when (repl-live? it)
	(with-current-buffer (or buf (current-buffer))
	  (repl-send-string it (thing-at-point thing t)))))

(cl-defmethod repl-send-sexp ((it repl) &optional buf)
  (repl-send-thing-at-point it 'sexp buf))

(cl-defmethod repl-send-defun ((it repl) &optional buf)
  (repl-send-thing-at-point it 'defun buf))

(cl-defmethod repl-send-C-c ((it repl) &optional buf)
  (repl-send-string it ""))

(cl-defmethod repl-send-C-l ((it repl) &optional buf)
  (repl-send-string it ""))

(defun repl--create-shell-functions ()
  (dolist (name '("start" "stop"
				  "hide" "stop"
				  "split" "split-right"
				  "send-region" "send-line" "send-buffer" "send-C-c" "send-C-l"))
	(eval (append@
		   (list 'defun
				 (intern (concat "repl-shell-" name))
				 nil
				 '(interactive))
		   (list (intern (concat "repl-" name))
				 (cget :repls 'shell))))))

(defun repl--create-mapping-functions ()
  (dolist (name '("start" "stop" "hide"
				  "split" "split-right"
				  "send-buffer" "send-line" "send-region"
				  "send-sexp" "send-defun" "send-C-c" "send-C-l"))
    (let* ((actual-fn (intern (concat "repl-" name))) 
           (fn-name (intern (concat "repl-mapping-" name)))
           (fn-root-name (intern (concat "repl-mapping-root-" name))))
	  (eval
	   `(defun ,fn-root-name (&optional buf)
		  (interactive)
		  (when-let* ((buf (or buf (current-buffer)))
					  (it (or (repl-buffer-get-repl buf 'shell)
							  (repl-init buf :shell t))))
			(,actual-fn it))))
	  (eval
	   `(defun ,fn-name (&optional buf)
		  (interactive)
		  (when-let* ((buf (or buf (current-buffer)))
					  (it (or (repl-buffer-get-repl buf 'workspace)
							  (repl-init buf))))
			(,actual-fn it)))))))

(defun repl--create-mappings ()
  (define-key! repl ()
    (normal
	 :prefix "SPC <return>"
     "<return>" 'repl-mapping-root-start
     "s" 'repl-mapping-root-split
     "C-c" 'repl-mapping-root-send-C-c
     "C-l" 'repl-mapping-root-send-C-l
     "v" 'repl-mapping-root-split-right
     "e" 'repl-mapping-root-send-line
     "b" 'repl-mapping-root-send-buffer
     "k" 'repl-mapping-root-hide
     "q" 'repl-mapping-root-stop
	 "." 'repl-mapping-root-send-sexp)
    (normal
	 :keymaps 'repl-mode-map
	 :prefix "SPC r"
     "C-c" 'repl-mapping-root-send-C-c
     "C-l" 'repl-mapping-root-send-C-l
     "r" 'repl-mapping-start
     "s" 'repl-mapping-split
     "v" 'repl-mapping-split-right
     "e" 'repl-mapping-send-line
     "b" 'repl-mapping-send-buffer
     "k" 'repl-mapping-hide
     "q" 'repl-mapping-stop
	 "." 'repl-mapping-send-sexp
	 "d" 'repl-mapping-send-defun)
    (visual
	 :keymaps 'repl-mode-map
	 :prefix "SPC"
     "re" 'repl-mapping-send-region
     "<return>e" 'repl-mapping-root-send-region))

  (define-key! repl-shell (:prefix "SPC x")
    (normal
     "x" 'repl-shell-start
     "C-c" 'repl-shell-send-C-c
     "C-l" 'repl-shell-send-C-l
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
