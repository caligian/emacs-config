(defun R-start (&optional buf)
  (interactive)
  (let* ((buf (or buf (current-buffer)))
		 (cwd (or (find-buffer-workspace buf)
				  (dirname buf)))
		 (proc (%. (local-config-get :R-processes) cwd)))
	(when (or (not proc)
			  (not (process-live? proc)))
	  (when-let* ((buf (R))
				  (buf-win (get-buffer-window buf))
				  (proc (get-buffer-process buf)))
		(%! (local-config-get :R-processes) cwd proc)
		(kbd! :states '(normal)
			  :keymaps 'local
			  "q" 'delete-window)
		proc))))

(defun R-get-buffer-process (&optional buf return-cwd?)
  (when-let ((buf (or buf (current-buffer)))
			 (ws (or (find-buffer-workspace buf)
					 (dirname buf)))
			 (proc (local-config-get :R-processes ws))
			 (live? (process-live? proc)))
	(if (not return-cwd?)
		proc
	  (list proc ws))))

(defun R-stop (&optional buf)
  (when-let* ((proc-and-cwd (R-get-buffer-process buf t))
			  (proc (car proc-and-cwd))
			  (cwd (last@ proc-and-cwd))
			  (proc-buf (process-buffer proc)))
	(stop-process proc)
	(%rm (local-config-get :R-processes) cwd)
	(when-let* ((win (get-buffer-window proc-buf)))
	  (delete-window win))))

(defun R-show (&optional buf direction)
  (when-let* ((direction (or direction :below))
			  (proc (R-get-buffer-process buf))
			  (proc-buf (process-buffer proc))
			  (not-displaying? (not (get-buffer-window proc-buf))))
	(pcase direction
	  (:below (split-window-below))
	  (:right (split-window-right)))
	(other-window 1)
	(switch-to-buffer proc-buf)))

(defun R-split-below (&optional buf)
  (interactive)
  (R-show buf :below))

(defun R-split-right (&optional buf)
  (interactive)
  (R-show buf :right))

(defun R-hide (&optional buf)
  (interactive)
  (when-let* ((proc-and-cwd (R-get-buffer-process buf t))
			  (proc (car proc-and-cwd))
			  (cwd (last@ proc-and-cwd))
			  (proc-buf (process-buffer proc))
			  (win (get-buffer-window proc-buf)))
	(delete-window win)))

(defun R-eval (&optional buf what)
  (when-let* ((buf (or buf (current-buffer)))
			  (proc (R-get-buffer-process buf))
			  (what (or what :region))
			  (text (concat (pcase what
							  (:region (buffer-get-region buf))
							  (:buffer (buffer2string buf))
							  (:line (buffer-current-line buf)))
							"\n")))
	(process-send-string proc text)))

(defun R-send (s &optional buf)
  (when-let* ((buf (or buf (current-buffer)))
			  (proc (R-get-buffer-process buf))
			  (s (if (list? s)
					 (string-join s "\n")
					 s)))
	(process-send-string proc (concat s "\n"))))

(defun R-eval-region (&optional buf)
  (interactive)
  (R-eval buf :region))

(defun R-eval-buffer (&optional buf)
  (interactive)
  (R-eval buf :buffer))

(defun R-eval-line (&optional buf)
  (interactive)
  (R-eval buf :line))

(defun R-goto-next-local (&optional buf)
  (interactive)
  (with-current-buffer (or buf (current-buffer))
	(when (search-forward-regexp "^[ ]*[^<]+<-" nil t)
	  (end-of-line))))

(defun R-goto-previous-local (&optional buf)
  (interactive)
  (with-current-buffer (or buf (current-buffer))
	(when (search-backward-regexp "^[ ]*[^<]+<-" nil t)
	  (beginning-of-line))))

(defun R-goto-next-function (&optional buf)
  (interactive)
  (with-current-buffer (or buf (current-buffer))
	(when (or (search-forward-regexp "<-[ ]*function" nil t)
			  (search-forward-regexp "<-[ ]*\\\\[(]" nil t))
	  (end-of-line)
	  t)))

(defun R-goto-previous-function (&optional buf)
  (interactive)
  (with-current-buffer (or buf (current-buffer))
	(when (or (search-backward-regexp "<-[ ]*function[(]" nil t)
			  (search-backward-regexp "<-[ ]*\\\\[(]" nil t))
	  (beginning-of-line)
	  (evil-next-line-1-first-non-blank)
	  t)))

(defun R-mark-function (&optional buf)
  (interactive)
  (let* ((buf (or buf (current-buffer))))
	(with-current-buffer buf
	  (when-let* ((line (buffer-current-line buf))
				  (function-line? (or (string-match-p "[ ]*<-[ ]*function[^{]+[{]" line)
									  (string-match-p "[ ]*<-[ ]*\\\\[(][^{]+[{]" line))))
		(evil-next-line-1-first-non-blank)
		(set-mark (point))
		(end-of-line)
		(search-backward-regexp "[{]")
		(evil-jump-item)
		(end-of-line)
		t))))

(defun R-mark-paragraph ()
  (interactive)
  (cl-labels ((jump-brackets (curline)
				(if-let* ((brackets? (=~ curline "[[{(]"))
						  (found? (progn
									(end-of-line)
									(search-backward-regexp "[[{(]" nil t))))
					(progn
					  (evil-jump-item)
					  (if-let* ((_curline (buffer-current-line))
								(sameline (equal _curline curline)))
						  curline
						(jump-brackets _curline)))
				  curline)))
	(let* ((curpoint (progn (beginning-of-line) (point)))
		   (endpoint nil)
		   (line (buffer-current-line))
		   (linenum (buffer-current-line-number))
		   (line (or (jump-brackets line) line))
		   (continue? t))
	  (setq continue? t)
	  (while continue?
		(if (=~ line "[|%]?[,>+][%]?[ ]*$")
			(progn
			  (next-line)
			  (setq line (buffer-current-line)))
		  (setq continue? nil)))

	  (end-of-line)
	  (setq endpoint (point))

	  (when (not (= curpoint endpoint))
		(goto-char curpoint)
		(set-mark curpoint)
		(goto-char endpoint)
		t))))

(defun R-eval-paragraph ()
  (interactive)
  (when (R-mark-paragraph)
	(R-send (buffer-get-region (current-buffer)))
	(deactivate-mark)))

(defun R-eval-function (&optional buf)
  (interactive)
  (when (R-mark-function buf)
	(R-send (buffer-get-region (or buf (current-buffer))) buf)))

(defun R-startup-directory (&optional buf)
  (let* ((buf (or buf (current-buffer)))
		 (ws (or (find-buffer-workspace buf)
				 (dirname buf))))
	ws))

(defun R-set-startup-directory (&optional buf)
  (with-current-buffer (or buf (current-buffer))
	(setq-local ess-startup-directory (R-startup-directory buf))))

(defun R-C-c (&optional buf)
  (interactive)
  (when-let* ((proc (R-get-buffer-process (or buf (current-buffer)))))
	(process-send-string proc "\n")))

(defun R-quit (&optional buf)
  (interactive)
  (when-let* ((buf (or buf (current-buffer)))
			  (proc (R-get-buffer-process buf))
			  (ws (or (find-buffer-workspace buf) (dirname buf))))
	(process-send-string proc "q()\nn\n")
	(let* ((procbuf (process-buffer proc))
		   (win (get-buffer-window procbuf)))
	  (when win
		(delete-window win))
	  (kill-buffer procbuf)
	  (local-config-set :R-processes ws nil))))

(defun R-package-p (&optional buf)
  (when-let* ((buf (or buf (current-buffer)))
			  (r-buf? (=~ (buffer-name buf) "[.]R$"))
			  (pkg-ws? (find-buffer-workspace buf (list "NAMESPACE" ".git")
											  nil :cached nil)))
	t))

(defalias 'R-package? 'R-package-p)

(defun R-load-devtools (&optional buf)
  (interactive)
  (when-let* ((proc (R-get-buffer-process buf))
			  (pkg? (R-package? buf)))
	(process-send-string proc (concat (kbd "C-c") "library(devtools)\n"))
	t))

(defun R-document-and-install (&optional buf)
  (interactive)
  (when-let* ((proc (R-get-buffer-process buf))
			  (pkg? (R-package? buf)))
	(process-send-string
	 proc
	 (concat (kbd "C-c") "library(devtools)\ndocument()\ninstall()\n\n"))
	t))
  

(defun R-install (&optional buf)
  (interactive)
  (when-let* ((proc (R-get-buffer-process buf))
			  (pkg? (R-package? buf)))
	(process-send-string proc (concat (kbd "C-c") "install()\n\n"))
	t))

(defun R-load-all (&optional buf)
  (interactive)
  (when-let* ((proc (R-get-buffer-process buf))
			  (pkg? (R-package? buf)))
	(process-send-string proc (concat (kbd "C-c") "load_all()\n"))
	t))

(defun R-document (&optional buf)
  (interactive)
  (when-let* ((proc (R-get-buffer-process buf))
			  (pkg? (R-package? buf)))
	(process-send-string proc (concat (kbd "C-c") "document()\n"))
	t))

(defun R-restart (&optional buf)
  (interactive)
  (when-let* ((proc (R-get-buffer-process buf)))
	(R-quit buf)
	(R-start buf)))

(add-hook 'ess-r-mode-hook 'R-set-startup-directory)

(define-key! R-mark-paragraph (:keymaps 'ess-r-mode-map)
  (visual "." 'R-mark-paragraph))

(define-key! R-edit (:prefix "C-c" :keymaps 'ess-r-mode-map)
  (normal
   "C-c" 'R-C-c
   "C-d" 'R-quit
   "C-l" 'R-load-all
   "C-p" 'R-goto-previous-function
   "C-n" 'R-goto-next-function
   "C-f" 'R-goto-next-local
   "C-b" 'R-goto-previous-local))
