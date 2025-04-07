(defun whereis (prog)
  (executable-find prog))

(defun shell-command-output (cmd)
  (let* ((out (shell-command-to-string cmd))
		 (out (string-split out "\n"))
		 (len (1- (length out)))
		 (out (if (equal "" (elt out len))
				  (butlast out 1)
				out)))
	out))

(defun chomp-path (p)
  (if (equal (substr p -1) "/")
	  (substr p 0 -1)
	p))

(defun join-path (&rest ps)
  (string-join ps "/"))

(defun dirname (s)
  (let* ((s (if (bufferp s) (buffer-file-name s) s)))
	(chomp-path (if (equal "/" (substr s -1))
					(file-name-directory (substr s 0 -1))
				  (file-name-directory s)))))

(defun basename (x)
  (concat (file-name-base x) "." (file-name-extension x)))

(defun list-files (dir &optional fullpath)
  (let* ((namelen (length dir))
		 (lastchar (substr dir (- namelen 1)))
		 (dir (if (and (equal lastchar "/") (> namelen 1))
				  (substr dir 0 -1)
				dir)))
    (--map
     (if fullpath
		 (if (equal dir "/")
			 (concat dir it)
		   (concat dir "/" it))
       it)
     (--filter (unless (or (equal it ".")
						   (equal it ".."))
				 it)
			   (directory-files dir)))))

(defun list-emacs-lisp-files (dir &optional fullpath)
  (thread-first (list-files dir fullpath)
				(filter@ (lambda (x)
						   (let* ((x (basename x)))
							 (and (=~ x "el$")
								  (!~ x "^[%#~.]" "[%#~.]$")))))))

(cl-defun path-exists-in-dir? (dir substrings &optional (depth 4))
  (let ((substrings (->list substrings)))
	(cl-labels ((file-exists? (d)
				  (cl-loop for s in substrings
						   when (file-exists-p (format "%s%s" d s))
						   return d))
				(recurse (d curdepth)
				  (cond
				   ((= curdepth (or depth 4))
					nil)
				   ((file-exists? d)
					d)
				   (t
					(recurse (file-name-parent-directory d) (1+ curdepth))))))
	  (recurse dir 0))))

(defalias 'path-exists-in-dir-p 'path-exists-in-dir?)

(cl-defun find-buffer-workspace (buf
								 &optional
								 (substrings '(".git"))
								 (depth 4)
								 (cached t)
								 map)
  (if-let* ((buf (or buf (current-buffer)))
			(use-cached? cached)
			(ws (cget :buffer-workspaces buf)))
	  ws
	(let* ((mm (buffer-major-mode (or buf (current-buffer))))
		   (mm-config (cget :modes mm))
		   (substrings (or substrings (%. mm-config 'workspace) '(".git")))
		   (depth (or depth (%. mm-config 'workspace-check-depth) 4))
		   (found (path-exists-in-dir? (dirname buf) substrings depth)))
	  (when found
		(cset :buffer-workspaces buf found)
		(cset :workspace-buffers found buf)
		(if map
			(funcall found (list :buffer buf :workspace map))
		  found)))))

(defun workspace-buffer? (ws buf)
  (when-let* ((buf (if (string? buf)
					   (get-buffer buf)
					 (and (buffer? buf) buf))))
	(%. workspace-buffers ws buf)))

(defalias 'workspace-buffer-p 'workspace-buffer?)

(defmacro if-in-workspace (buf &rest body)
  (declare (indent 1))
  `(when-let* ((workspace (find-buffer-workspace ,buf)))
	 ,@body))
