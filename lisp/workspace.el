(setq buffer-file-name-workspaces (ht))
(setq buffer-file-name-cwds (ht))

(defun find-path-workspace (path &optional pats depth)
  (path-exists-in-subdir? path
			  (or pats '(".git"))
			  (or depth 4)))

(defun dirname (&optional buf)
  (let* ((exists? (get% buffer-file-name-cwds buf)))
    (if exists?
	exists?
      (let* ((buf (or buf (current-buffer)))
	     (name (buffer-file-name buf))
	     (dir (dirname name)))
	(set% buffer-file-name-cwds name dir)))))

(defun find-buffer-workspace (&optional buf pats depth)
  (let* ((exists? (get% buffer-file-name-workspaces buf)))
    (if exists?
	exists?
      (let* ((buf (or buf (current-buffer)))
	     (name (buffer-file-name buf))
	     (dir (dirname name))
	     (workspace (find-path-workspace dir pats depth)))
	(when workspace
	  (set% buffer-file-name-workspaces (buffer-file-name buf) workspace)
	  workspace)))))
