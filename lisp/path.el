(setq path-lookup-alist '((HOME . (getenv "HOME"))
						  (ROOT . "/")
						  (config "~/.emacs.d/")
						  (state "~/.local/state")
						  (xdg-config "~/.config")
						  (bashrc "~/.bashrc")
						  (zshrc "~/.zshrc")
						  (shell "/usr/bin/zsh")))

(defun whereis (prog)
  (executable-find prog))

(cl-defun shell-command! (cmd &optional (as-list t))
  (let* ((out (shell-command-to-string cmd))
		 (out (if as-list
				  (string-split out "\n")
				out))
		 (out (if (list? out)
				  (cl-loop for x in out
						   when (not (= (length x) 0))
						   collect x)
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

(defun find-buffer-workspace (buf &optional substrings depth)
  (if-let* ((ws (local-config-get :buffer-workspaces buf)))
	  ws
	(let* ((mm (buffer-major-mode buf))
		   (mm-config (local-config-get :modes mm))
		   (substrings (or substrings (%. mm-config 'workspace) '(".git")))
		   (depth (or depth (%. mm-config 'workspace-check-depth) 4))
		   (found (path-exists-in-subdir? (dirname buf) substrings depth)))
	  (when found
		(local-config-set :buffer-workspaces buf found)
		(local-config-set :workspace-buffers found buf)
		found))))

(defun workspace-buffer? (ws buf)
  (when-let* ((buf (if (string? buf)
					   (get-buffer buf)
					 (and (buffer? buf) buf))))
	(%. workspace-buffers ws buf)))

(defalias 'workspace-buffer-p 'workspace-buffer?)

(defmacro path* (&rest words)
  `(replace-regexp-in-string
	"/+" "/" (string-join
			  (cl-loop for word in ',words
					   collect (if-let* ((expansion (%. ',path-lookup-alist word)))
								   (if (list? expansion)
									   (eval expansion)
									 expansion)
								 (cond
								  ((list? word)
								   (eval word))
								  ((and (symbol? word)
										(=~ (symbol-name word) "^[A-Z-]+"))
								   (getenv (symbol-name word)))
								  ((string? word)
								   word))))
			  "/")))

(defmacro path! (words)
  `(path* ,@words))
