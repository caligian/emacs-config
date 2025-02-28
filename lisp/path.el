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


(defun path-exists-in-dir? (dir &rest substrings)
  (cl-labels ((exists? (r fs)
				(if-let* ((f (car fs)))
					(if (cl-search r f)
						t
					  (exists? r (cdr fs))))))
    (if-let* ((r (car substrings)))
		(if (exists? r (if (listp dir) dir (list-files dir t)))
			dir
		  (apply #'path-exists-in-dir? dir (cdr substrings))))))

(defalias 'path-exists-in-dir-p 'path-exists-in-dir?)

(defun path-exists-in-subdir? (dir substrings &optional depth current-depth)
  (let* ((depth (or depth 5))
		 (current-depth (or current-depth 1)))
	(when (and (< current-depth depth)
			   (not (equal dir "/")))
	  (if (not (apply #'path-exists-in-dir? dir substrings))
		  (path-exists-in-subdir?
		   (dirname dir)
		   substrings
		   depth
		   (+ current-depth 1))
		dir))))

(defalias 'path-exists-in-subdir-p 'path-exists-in-subdir?)

(cl-defun find-buffer-workspace (buf &optional substrings depth &key (cached t) map)
  (if-let* ((buf (or buf (current-buffer)))
			(use-cached? cached)
			(ws (local-config-get :buffer-workspaces buf)))
	  ws
	(let* ((mm (buffer-major-mode (or buf (current-buffer))))
		   (mm-config (local-config-get :modes mm))
		   (substrings (or substrings (%. mm-config 'workspace) '(".git")))
		   (depth (or depth (%. mm-config 'workspace-check-depth) 4))
		   (found (path-exists-in-subdir? (dirname buf) substrings depth)))
	  (when found
		(local-config-set :buffer-workspaces buf found)
		(local-config-set :workspace-buffers found buf)
		(if map
			(funcall found (list :buffer buf :workspace map))
		  found)))))

(cl-defun find-buffer-workspace (buf
								 &optional
								 (substrings '(".git"))
								 (depth 4)
								 &key
								 (cached t)
								 map)
  (let* ((buf (or buf (current-buffer)))
		 (exists (and cached (local-config-get :buffer-workspaces buf))))
	(if exists
		exists
	  (when-let* ((mm-config (local-config-get :modes (buffer-major-mode buf)))
				  (substrings (or substrings (%. mm-config 'workspace) '(".git")))
				  (depth (or depth (%. mm-config 'workspace-check-depth) 4))
				  (found (path-exists-in-subdir? (dirname buf) substrings depth)))
		(when found
		  (local-config-set :buffer-workspaces buf found)
		  (local-config-set :workspace-buffers found buf)
		  (if map
			  (funcall found (list :buffer buf :workspace map))
			found))))))

(defun workspace-buffer? (ws buf)
  (when-let* ((buf (if (string? buf)
					   (get-buffer buf)
					 (and (buffer? buf) buf))))
	(%. workspace-buffers ws buf)))

(defalias 'workspace-buffer-p 'workspace-buffer?)

(defun make-path (lookup-alist &rest words)
  (let* ((lookup-alist (append lookup-alist (local-config-get :path-lookup-alist)))
		 (expanded (cl-loop for word in words
							collect (cond
									 ((stringp word)
									  word)
									 ((listp word)
									  (eval word))
									 ((symbolp word)
									  (if-let* ((exists (%. lookup-alist word)))
										  (if (listp exists) (eval exists) exists)
										(error "SYMBOL does not exist in :path-lookup-alist: %s" word)))
									 (t (error "FORM does not eval to string: %s" word))))))
	(replace-regexp-in-string
	 "/\\{2,\\}" "/"
	 (string-join expanded "/"))))

(cl-defmacro path* (words &key lookup-alist)
  (apply 'make-path lookup-alist words))

(defmacro if-in-workspace (buf &rest body)
  (declare (indent 1))
  `(when-let* ((workspace (find-buffer-workspace ,buf)))
	 ,@body))
