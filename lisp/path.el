(defun chomp-path (p)
  (if (equal (substr1 p -1) "/")
      (substr p 0 -1)
    p))

(defun join-path (&rest ps)
  (string-join ps "/"))

(defun dirname (s)
  (let* ((s (if (bufferp s) (buffer-file-name s) s)))
    (chomp-path (if (equal "/" (substr1 s -1))
		    (file-name-directory (substr s 0 -1))
		  (file-name-directory s)))))

(defun basename (x)
  (concat (file-name-base x) "." (file-name-extension x)))

(defun list-files (dir &optional fullpath)
  (let* ((namelen (length dir))
	 (lastchar (substr1 dir (- namelen 1)))
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
				  (not=~ x "^[%#~.]" "[%#~.]$")))))))


(defun path-exists-in-dir? (dir &rest regexes)
  (cl-labels ((exists? (r fs)
		(if-let* ((f (car fs)))
		    (if (string-match-p r f)
			t
		      (exists? r (cdr fs))))))
    (if-let* ((r (car regexes)))
	(if (exists? r (if (listp dir) dir (list-files dir t)))
	    dir
	  (apply #'path-exists-in-dir? dir (cdr regexes))))))

(defun path-exists-in-subdir? (dir regexes &optional depth current-depth)
  (let* ((depth (or depth 5))
	 (current-depth (or current-depth 1)))
    (when (and (< current-depth depth)
	       (not (equal dir "/")))
      (if (not (apply #'path-exists-in-dir? dir regexes))
	  (path-exists-in-subdir?
	   (dirname dir)
	   regexes
	   depth
	   (+ current-depth 1))
	dir))))

(cl-defun find-buffer-workspace (buf &optional regex depth)
  (path-exists-in-subdir? (dirname buf)
			  (or regex (list "\\.git"))
			  (or depth 4)))

