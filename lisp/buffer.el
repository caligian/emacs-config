(setq buffer-workspaces (ht))

(defun buffer-get-region (buf)
  (with-current-buffer buf
    (buffer-substring-no-properties (mark) (point))))

(defun buffer-prepend (buf &rest strings)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (dolist (s strings)
	(insert s)))))

(defun buffer-append (buf &rest strings)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-max))
      (dolist (s strings)
	(insert s)))))

(defun buffer-prepend-at-line (buf line strings)
  (with-current-buffer buf
    (save-excursion
      (goto-line line)
      (beginning-of-line)
	  (dolist (s strings)
		(insert s)))))

(defun buffer-append-at-line (buf line strings)
  (with-current-buffer buf
    (save-excursion
      (goto-line line)
      (end-of-line)
      (dolist (s strings)
	(insert s)))))

(defun buffer-line-count (buf)
  (with-current-buffer buf
    (count-lines (point-min) (point-max))))

(defun buffer-get-line (buf linenum)
  (with-current-buffer buf
    (save-excursion
      (goto-line linenum)
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (deactivate-mark)
      (buffer-get-region buf))))

(defun buffer-create-scratch (buf)
  (let* ((buf (or buf (get-buffer-create (make-temp-name "user-temp-buffer-")))))
    (with-current-buffer buf
      (general-define-key :keymaps 'local "q" :states 'normal 'kill-buffer-and-window))
    buf))

(defun buffer-create-global-scratch ()
  (let* ((buf (find-file-noselect "~/.emacs.d/scratch")))
    (with-current-buffer buf
      (general-define-key :keymaps 'local  :states 'normal "q" 'kill-buffer-and-window))
    buf))

(defun buffer-split (buf direction)
  (with-current-buffer (current-buffer)
    (pcase direction
      ('split (split-window-below)
	      (other-window 1)
	      (switch-to-buffer buf))
      ('vsplit (split-window-right)
	       (other-window 1)
	       (switch-to-buffer buf))
      ('frame (switch-to-buffer-other-frame buf))
      ('window (switch-to-buffer-other-window buf)))))

(defun buffer2string (buf &optional start-point end-point)
  (with-current-buffer (or buf (current-buffer))
    (buffer-substring-no-properties
     (or start-point (point-min))
     (or end-point (point-max)))))

(defalias 'buffer-contents 'buffer2string)

(defun buffer-set-string (buf string)
  (let* ((buf (or buf (current-buffer))))
    (save-excursion
      (beginning-of-buffer)
      (set-mark (point))
      (end-of-buffer)
      (deactivate-mark)
      (kill-region (mark) (point))
      (insert string))))

(defun buffer-major-mode (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    major-mode))

(defun buffer-current-line-number (&optional buf)
  (let* ((buf (or buf (current-buffer)))
	 (linenum (with-current-buffer buf (line-number-at-pos))))
    linenum))

(defun buffer-current-line (&optional buf)
  (let* ((buf (or buf (current-buffer))))
   (buffer-get-line buf (buffer-current-line-number buf))))

(defun scratch-buffer-split (&optional direction)
  (let* ((direction (or direction :right))
	 (buf (buffer-create-global-scratch))
	 (win (get-buffer-window buf)))
    (when (not win)
      (with-current-buffer (current-buffer)
	(if (eq direction :right)
	    (split-window-horizontally)
	  (split-window-vertically))
	(other-window 1)
	(switch-to-buffer buf)))))

(defun scratch-buffer-below ()
  (interactive)
  (funcall #'scratch-buffer-split :below))

(defun scratch-buffer-right ()
  (interactive)
  (funcall #'scratch-buffer-split :right))

(defun buffer-replace-region (buf new-region)
  (with-current-buffer (or buf (current-buffer))
    (delete-region (mark) (point))
    (insert new-region)))

(defun buffer-delete-region (buf)
  (with-current-buffer (or buf (current-buffer))
    (delete-region (mark) (point))))

(defun buffer-regexp-match (regex)
  (let* ((bufs (buffer-list))
		 (buf-names (map@ bufs 'buffer-name))
		 (buf-names (filter@ buf-names (lambda (name) (string-match-p regex name))))
		 (bufs (map@ buf-names 'get-buffer)))
	bufs))

(defun buffer-regexp-exists? (regex)
  (cl-loop for buf in (buffer-list)
		   when (string-match-p regex (buffer-name buf))
		   return t))

(cl-defun buffer-get-lines (buf &optional start-row end-row)
  (when-let* ((lc (buffer-line-count buf))
			  (with-offset (lambda (i)
							 (cond
							  ((< i 0) (+ lc i))
							  ((<= i lc) i)
							  (t nil))))
			  (start-row (funcall with-offset (or start-row 1)))
			  (end-row (funcall with-offset (or end-row -1))))
	(with-current-buffer buf
	  (save-excursion
		(goto-line start-row)
		(beginning-of-line)
		(set-mark (point))
		(goto-line end-row)
		(end-of-line)
		(deactivate-mark)
		(buffer-get-region buf)))))
