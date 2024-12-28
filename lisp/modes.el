(setq mode-config-config-path "~/.emacs.d/modes")
(setq mode-configs (ht))
(setq repl-input-files (list))

(defclass mode-config ()
  (;; builtin default project config options
   (project-file
    :initarg :project-file
    :initform nil
    :documentation "")
   (compilation-dir
    :initarg :compilation-dir
    :initform nil
    :documentation "")
   (compile
    :initarg :compile
    :initform nil
    :documentation "")
   (configure
    :initarg :configure
    :initform nil
    :documentation "")
   (install
    :initarg :install
    :initform nil
    :documentation "")
   (package
    :initarg :package
    :initform nil
    :documentation "")
   (run
    :initarg :run
    :initform nil
    :documentation "")
   (src-dir
    :initarg :src-dir
    :initform nil
    :documentation "")
   (test
    :initarg :test
    :initform nil
    :documentation "")
   (test-dir
    :initarg :test-dir
    :initform nil
    :documentation "")
   (test-prefix
    :initarg :test-prefix
    :initform nil
    :documentation "")
   (test-suffix
    :initarg :test-suffix
    :initform nil
    :documentation "")
   (related-files-fn
    :initarg :related-files-fn
    :initform nil
    :documentation "")

   ;; configure workspace detection
   (workspace
    :initarg :workspace
    :initform '(".git") 
    :documentation "root directory patterns")
   (workspace-check-depth
    :initarg :workspace-check-depth
    :initform 5
    :documentation "depth for getting root directory")

   (id
    :initarg :id
    :initform nil
    :documentation "associated mode")

   ;; builtin-formatter
   (formatter
    :initarg :formatter
    :initform nil
    :documentation "code formatter commands")

   ;; mode-specific maps and hooks
   (mode-map
    :initarg :mode-map
    :initform nil
    :documentation "mode-map for major-mode")
   (mode-hook
    :initarg :mode-hook
    :initform nil
    :documentation "hook for major-mode")

   ;; builtin-repl configuration
   (repl
    :initarg :repl
    :initform '()
    :documentation "REPL command definitions")
   (repl-use-input-file
    :initarg :repl-use-input-file
    :initform nil
    :documentation "use an input file instead of directly sending input. function should return the eval filename string, eg %load 'input_filename.py' ")
   (repl-input-filter
    :initarg :repl-input-filter
    :initform s
    :documentation "modify input before sending it to REPL. This is run before repl-use-input-file")
   (repl-help
    :initarg :repl-help
    :initform nil
    :documentation "function accepting a string describing sexp. This will be sent to running repl")

   ;; lsp config for eglot
   (lsp
    :initarg :lsp
    :initform nil
    :documentation "lsp server")

   ;; default mappings
   (map
    :initarg :map
    :initform nil
    :documentation "mappings")

   ;; enable builtin-terminal repl
   (builtin-terminal
    :initarg :builtin-terminal
    :initform nil
    :documentation "use terminal that comes with this config")

   ;; default hooks
   (hooks
    :initarg :hooks
    :initform nil
    :documentation "all hooks for this name")

   ;; register builtin project configuration?
   (builtin-project
    :initarg :builtin-project
    :initform nil
    :documentation "enable builtin project configuration via projectile?")

   ;; enable custom project configuration
   (enable-project
    :initarg :enable-project
    :initform nil
    :documentation "enable project configuration via projectile?")
   ;; 'projectile-register-project-type form(s)
   (project
    :initarg :project
    :initform nil
    :documentation "project configs for this mode")))

(defmacro mode-config-defun (name args &rest body)
  (declare (indent 2))
  (let* ((name (concat "mode-config/" (symbol-name name)))
	 (name (intern name)))
    `(defun ,name ,args ,@body)))

(cl-defmethod mode-config-register-default-project ((obj mode-config))
  (with-slots (workspace id)
      obj
    (let* ((name (intern (concat (symbol-name id) "-default")))
	   (vars '(project-file compilation-dir compile configure install package run src-dir test test-dir test-prefix test-suffix related-files-fn workspace id))
	   (args (list name workspace))
	   (keywords (map@ vars (lambda (x) (concat ":" (symbol-name x)))))
	   (i 0))
      (dolist (var vars)
	(when-let* ((value (%. obj var)))
	  (setq args (append@ args (nth i keywords) value)))
	(setq i (1+ i)))
      (apply 'projectile-register-project-type args))))

(cl-defmethod mode-config-register-project ((obj mode-config))
  (with-slots (project) obj
    (if (list? (first@ project))
	(dolist (config project)
	  (apply 'projectile-register-project-type config))
     (apply 'projectile-register-project-type project))))

(cl-defmethod mode-config-define-key ((obj mode-config) states &rest args)
  (let* ((mode-map (%. obj 'mode-map))
	 (args (lappend@ args
			 :keymaps mode-map
			 :states (->list (or states 'normal)))))
    (apply #'general-define-key args)))

(defmacro mode-config-define-keys (obj &rest forms)
  `(cl-loop for form in ',forms
	    do (apply 'mode-config-define-key
		      ,obj
		      (car form)
		      (cdr form))))

(defmacro mode-config-add-hook (obj &rest body)
  `(add-hook (%. ,obj 'mode-hook) (lambda nil ,@body)))

(defalias 'mode-config-add-hooks 'mode-config-add-hook)

(cl-defmethod mode-config-set-mappings ((obj mode-config))
  (cl-loop for m in (%. obj 'map)
	   do (apply 'mode-config-define-key obj (car m) (cdr m))))

(defun mode-config-set-hooks (obj)
  (let* ((hooks (%. obj 'hooks))
	 (form (list 'mode-config-add-hooks obj))
	 (form (append form hooks)))
    (eval form)))

(defun mode-config-load-file (mode)
  (when-let* ((fname (concat "~/.emacs.d/modes/" mode ".el"))
	      (exists? (file-exists-p fname)))
    (load-file fname)))

(cl-defmethod mode-config-query ((mode mode-config) attrib &optional type)
  (when (contains?@ (list 'compile 'repl 'formatter) attrib)
    (let* ((attrib-value (%. mode attrib))
	   (value (if (eq attrib 'formatter)
		      attrib-value
		    (if type
			(last@ (assoc type attrib-value))
		      attrib-value))))
      value)))

(defun mode-config-query! (buf-or-symbol-or-mode attrib &optional type)
  (when-let* ((mode (cond
		   ((bufferp buf-or-symbol-or-mode)
		    (buffer-mode-config buf-or-symbol-or-mode))
		   ((symbolp buf-or-symbol-or-mode)
		    (get% mode-configs buf-or-symbol-or-mode))
		   (t buf-or-symbol-or-mode))))
    (mode-config-query mode attrib type)))

(cl-defmethod mode-config-find-workspace ((obj mode-config) &optional buf)
  (apply 'find-buffer-workspace
	 (or buf (current-buffer))
	 (list (%. obj 'workspace)
	       (%. obj 'workspace-check-depth))))

(defun query-mode-config (src &rest attribs)
  (let* ((mode (cond
		((buffer? src)
		 (with-current-buffer src major-mode))
		((symbol? src)
		 src)
		(t src)))
	 (obj (if (object? mode) mode (%. mode-configs mode))))
    (apply #'%. obj attribs)))

(defun buffer-mode-config (src &rest attribs)
  (with-current-buffer src
    (apply 'query-mode-config major-mode attribs)))

(defun mode-config--command-default-subs (src-buffer type cmd)
  (cl-flet* ((put-env (x)
	       ()))))

(string-match "\\$hello" "hello")

(defun mode-config-command (src &rest attribs))

(cl-defmethod mode-config-buffer-command (buf (obj mode-config) attrib &optional type)
  (when-let* ((buf (or buf (current-buffer)))
	      (type (or type 'buffer))
	      (value (mode-config-query obj attrib (or type 'buffer)))
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

(defun mode-config-buffer-command! (buf buf-or-symbol-or-mode attrib type)
  (when-let* ((mode (cond
		   ((bufferp buf-or-symbol-or-mode)
		    (buffer-mode-config buf-or-symbol-or-mode))
		   ((symbolp buf-or-symbol-or-mode)
		    (get% mode-configs buf-or-symbol-or-mode))
		   (t buf-or-symbol-or-mode))))
    (mode-config-buffer-command buf mode attrib type)))

(defun mode-config-get-buffer-mode-config (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (get% mode-configs major-mode)))

(defun mode-config-list-modes (&optional fullpath)
  (if fullpath
      (list-emacs-lisp-files "~/.emacs.d/modes" t)
    (-> (list-emacs-lisp-files "~/.emacs.d/modes")
	(map@ (lambda (x) (replace-regexp-in-string "\\.el$" "" x))))))

(defun mode-config-load-directory ()
  (each@ (list-emacs-lisp-files "~/.emacs.d/modes" t)
	 (lambda (x)
	   (message "loading mode-config config from %s" x)
	   (load-file x))))

(defalias 'buffer-mode-config 'mode-config-get-buffer-mode-config)

(defun mode-config--repl-delete-input-files ()
  (each@ repl-input-files 'delete-file))

(defmacro mode-config! (&rest args)
  (let* ((parsed (gensym))
		 (mm (gensym))
		 (mm-name (gensym))
		 (hook (gensym))
		 (map (gensym))
		 (repl-input-filter-forms (gensym))
		 (repl-input-filter (gensym)))
	`(let* ((,parsed (last@ (parse-arguments! ,@args)))
			(,parsed (map% ,parsed (lambda (_ v)
									 (if (and (singleton? v)
											  (not (list? (first@ v))))
										 (first@ v)
									   v))))
			(,parsed (->plist% ,parsed))
			(,parsed (apply #'mode-config ,parsed))
			(,mm (%. ,parsed 'id))
			(,mm-name (symbol-name ,mm))
			(,hook (intern (concat ,mm-name "-hook")))
			(,map (intern (concat ,mm-name "-" "map")))
			(,repl-input-filter-forms
			 (%. ,parsed 'repl-input-filter))
			(,repl-input-filter
			 (eval (append '(lambda (s it)) (->list ,repl-input-filter-forms)))))

       (%setq ,parsed
			  'mode-hook ,hook
			  'mode-map ,map
			  'repl-input-filter ,repl-input-filter)

       (%! mode-configs ,mm ,parsed)

       (mode-config-set-hooks ,parsed)
       (mode-config-set-mappings ,parsed)

       (when (%. ,parsed 'builtin-project)
		 (mode-config-register-default-project ,parsed))

       (when (%. ,parsed 'enable-project)
		 (mode-config-register-project ,parsed))

       (when (%. ,parsed 'builtin-terminal)
		 (add-hook ,hook 'repl-mode)
		 (repl-create-mappings))

       ,parsed)))


(run-at-time t 70 'mode-config--repl-delete-input-files)

(mode-config! :id shell)
