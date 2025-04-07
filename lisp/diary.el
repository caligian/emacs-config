; Simple utility functions to maintain a diary in a single file

(defun simple-diary-goto-previous-date ()
  (interactive)
  (re-search-backward (concat "^[*]+ +[0-9]+/[0-9]+/2[0-9]+")))

(defun simple-diary-goto-next-date ()
  (interactive)
  (re-search-forward (concat "^[*]+ +[0-9]+/[0-9]+/2[0-9]+")))

(defun simple-diary-open ()
  (interactive)
  (let* ((buf (find-file-noselect (or (cget :diary-path) "~/diary.org"))))
	(cset :diary-buffer buf)
	(switch-to-buffer-other-frame buf)))

(defun simple-diary-today ()
  (interactive)
  (end-of-buffer)
  (let* ((today (format-time-string "%d/%m/%Y")))
	(unless (re-search-backward (concat "^[*] " today) nil t)
	  (end-of-buffer)
	  (insert "\n")
	  (insert (concat "* " today))
	  (insert "\n")
	  (beginning-of-line-text))))
