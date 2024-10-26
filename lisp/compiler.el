(defun compilation--unique-buffer-name (_)
  (compilation--default-buffer-name (make-temp-name "async-compile-")))

(setq compilation-buffer-name-function #'compilation--unique-buffer-name)

(defun compiler-compile-buffer (&optional buf type cmd)
  (interactive)
  (when-let* ((buf (or buf (current-buffer)))
	      (fname (buffer-file-name buf))
	      (type (or type 'buffer))
	      (cmd (if cmd
		       cmd
		     (lang-buffer-command! buf buf 'compile type)))
	      (cmd (if (string-match-p "%buffer" cmd)
		       (replace-regexp-in-string "%buffer" fname cmd)
		     cmd)))
    (compile cmd)))

(defun compiler-compile-workspace (&optional buf cmd)
  (interactive)
  (compiler-compile-buffer buf 'workspace cmd))

(defalias 'compile-buffer 'compiler-compile-buffer)
(defalias 'compile-workspace-buffer 'compiler-compile-workspace)
