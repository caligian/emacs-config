(setq lang-config-path "~/.emacs.d/ft")
(setq langs (ht))

(defclass lang ()
  ((workspace :initarg :workspace
	      :initform '("\\.git") 
	      :documentation "root directory patterns")
   (workspace-check-depth :initarg :workspace-check-depth
			  :initform 5
			  :documentation "depth for getting root directory")
   (major-mode :initarg :major-mode
	       :initform nil
	       :documentation "associated major mode")
   (formatter :initarg :formatter
	      :initform nil
	      :documentation "code formatter commands")
   (repl :initarg :repl
	 :initform '()
	 :documentation "REPL command definitions")
   (compile :initarg :compile
	    :initform '()
	    :documentation "compilation commands")
   (lsp :initarg :lsp
	:initform nil
	:documentation "lsp server")
   (map :initarg :map
	:initform nil
	:documentation "mappings")
   (builtin-terminal :initarg :builtin-terminal
		     :initform nil
		     :documentation "use terminal that comes with this config")
   (hooks :initarg :hooks
	  :initform nil
	  :documentation "all hooks for this name")))


(defun make-lang (&rest args)
  (let* ((obj (from-plist% args)))
    obj))

(make-lang :formatter "python3" :major-mode 'python-mode)

(cl-defmethod lang-set-mappings ((obj lang))
  (let* ((mode (%. obj 'major-mode))
	 (mode-map (intern (concat (symbol-name mode) "-map")))
	 (map (%. obj 'map))
	 (map (if (listp (car map))
		  map
		`((,(car map) ,@(cdr map))))))
    (each@ map (lambda (x)
		 (let* ((states (car x))
			(states (list states)))
		   (eval `(general-define-key :states ',states
					      :keymaps ',mode-map
					      ,@(cdr x))))))
    t))

(cl-defmethod lang-add-hooks ((obj lang))
  (let* ((hook (intern (concat (symbol-name (%. obj 'major-mode)) "-hook")))
	 (hooks (%. obj 'hooks))
	 (fn `(lambda nil ,@hooks)))
    (add-hook hook (eval fn))))

(defun lang-load-file (ft)
  (when-let* ((fname (concat "~/.emacs.d/ft/" ft ".el"))
	      (exists? (file-exists-p fname)))
    (load-file fname)))

(cl-defmethod lang-query ((ft lang) attrib &optional type)
  (when (contains?@ (list 'compile 'repl 'formatter) attrib)
    (let* ((attrib-value (%. ft attrib))
	   (value (if (eq attrib 'formatter)
		      attrib-value
		    (if type
			(last@ (assoc type attrib-value))
		      attrib-value))))
      value)))

(defun lang-query! (buf-or-symbol-or-ft attrib &optional type)
  (when-let* ((ft (cond
		   ((bufferp buf-or-symbol-or-ft)
		    (buffer-lang buf-or-symbol-or-ft))
		   ((symbolp buf-or-symbol-or-ft)
		    (get% langs buf-or-symbol-or-ft))
		   (t buf-or-symbol-or-ft))))
    (lang-query ft attrib type)))

(cl-defmethod lang-buffer-command (buf (obj lang) attrib &optional type)
  (when-let* ((buf (or buf (current-buffer)))
	      (type (or type 'buffer))
	      (value (lang-query obj attrib (or type 'buffer)))
	      (path (pcase type
		      ('workspace (find-buffer-workspace
				   buf
				   (%. obj 'workspace)
				   (%. obj 'workspace-check-depth)))
		      ('cwd (dirname buf))
		      ('buffer (buffer-file-name buf))))
	      (value (if (string-match-p "%buffer" path)
			 (replace-regexp-in-string "%buffer" (buffer-file-name buf) value)
		       value)))
    (replace-regexp-in-string "%path" path value)))

(defun lang-buffer-command! (buf buf-or-symbol-or-ft attrib type)
  (when-let* ((ft (cond
		   ((bufferp buf-or-symbol-or-ft)
		    (buffer-lang buf-or-symbol-or-ft))
		   ((symbolp buf-or-symbol-or-ft)
		    (get% langs buf-or-symbol-or-ft))
		   (t buf-or-symbol-or-ft))))
    (lang-buffer-command buf ft attrib type)))

(defmacro lang! (&rest args)
  (let* ((obj (apply #'lang args))
	 (mm (%. obj 'major-mode)))
    (set% langs mm obj)
    (lang-add-hooks obj)
    (lang-set-mappings obj)
    obj))

(defun lang-get-buffer-lang (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (get% langs major-mode)))

(defun lang-load-directory ()
  (-> (list-files lang-config-path)
      (filter@
       (lambda (x) (when (string-match-p "^[a-zA-Z0-9]*.el" x) x))
       (lambda (x) (concat lang-config-path "/" x)))
      (each@ #'load-file)))

(defalias 'buffer-lang 'lang-get-buffer-lang)
