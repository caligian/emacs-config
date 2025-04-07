(require 'apheleia)
(require 'projectile)

(load-file "~/.emacs.d/lisp/async-process.el")

;; compile definition
;;  valid words: command cmd path filepath workspace dir home
;;  is a list in the form
;;  (SYMBOL|STRING . EXPRESSION) where words above will be substituted

;; repl definition
;;	help
;;  input-filter

;; format definition 
;; FORM: (SYMBOL EXPRESSION) where args is a a form or string

;; project definition:
;; FORM: (SYMBOL ...rest)
;;	root-dir (list STRING)
;;	project-file 
;;	compilation-dir
;;	local-configure
;;	compile  
;;	test
;;	install
;;	package
;;	run
;;	test-prefix
;;	test-suffix
;;	src-dir
;;	test-dir
;;	related-files-fn

(class mode-config ()
  mode
  (workspace (list ".git"))
  (workspace-check-depth 4)
  compile
  project format
  builtin-terminal
  hooks mappings hook map
  package
  repl)

(defun mode-config--substitute (exp buf)
  (let* ((fname (buffer-file-name buf))
		 (dir (dirname buf))
		 (ws (find-buffer-workspace buf))
		 (defaults `((home			,(getenv "HOME"))
					 (filepath		,fname)
					 (path			,fname)
					 (cwd			,dir)
					 (currentdir	,dir)
					 (dir			,dir)
					 (root			,ws)
					 (workspace		,ws))))
	(string-join (expression exp defaults) " ")))

(defun mode-config-add-projects (conf)
  (cl-loop for x in (%. conf 'project)
		   do (let* ((x (cdr (from-plist% x)))
					 (project-type (car x))
					 (marker-files (%. x 'root-dir))
					 (args (as-plist% (reject% x (list :name :root-dir))))
					 (form (list project-type marker-files))
					 (args (append form args)))
				(apply 'projectile-register-project-type args))))

(defun mode-config-define-keys (conf)
  (let* ((map (%. conf 'map))
		 (mappings (%. conf 'mappings)))
	(cl-loop for x in mappings
			 do (let* ((state (car x))
					   (args (append `(general-define-key :states ',state :keymaps ',map) (cdr x))))
				  (eval args)))))

(defun mode-config-add-hooks (conf)
  (let* ((m (%. conf 'mode))
		 (hook (%. conf 'hook))
		 (hooks (%. conf 'hooks))
		 (fn-name (concat "mode-config-" (symbol-name m) "-hook-function"))
		 (fn-name (intern fn-name))
		 (fn (append `(defun ,fn-name ()) hooks)))
	(eval fn)
	(add-hook hook fn-name)))

(defun mode-config-add-formatters (conf)
  (cl-loop for x in (%. conf 'format)
		   do (let* ((mode (%. conf 'mode))
					 (defined (->list (%. apheleia-mode-alist 'mode)))
					 (name (car x))
					 (args (cdr x)))
				(cl-pushnew name defined)
				(setf (alist-get mode apheleia-mode-alist) defined)
				(cl-pushnew `(,name ,@args) apheleia-formatters))))

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

(defun buffer-mode-config (&optional buf)
  (let* ((buf (or buf (current-buffer))))
	(local-config-get :modes (buffer-major-mode buf))))

(defmacro mode! (m &rest args)
  (declare (indent 1))
  (cl-with-gensyms (parsed
					compile
					map
					hook
					repl
					ws
					ws-check-depth
					repl-input-filter-forms
					repl-input-filter
					repl-help)
	`(let* ((,parsed (parse-arguments! ,@args))
			(,parsed (cadr ,parsed))
			(,parsed (->plist% ,parsed))
			(,parsed (append (list :mode ',m) ,parsed))
			(,parsed (apply 'mode-config ,parsed))
			(,hook (intern (format "%s-hook" (symbol-name ',m))))
			(,map (intern (format "%s-map" (symbol-name ',m))))
			(,ws (or (%. ,parsed 'workspace) (list ".git")))
			(,ws-check-depth (or (%. ,parsed 'workspace-check-depth) 4)))

	   (%setq ,parsed 'hook ,hook 'map ,map)

	   (mode-config-add-projects ,parsed)
	   ;; (mode-config-add-formatters ,parsed)
	   (mode-config-define-keys ,parsed)
	   (mode-config-add-hooks ,parsed)

	   (local-config-set :modes ',m ,parsed)

	   (when (%. ,parsed 'builtin-terminal)
		 (add-hook ,hook 'repl-mode))

	   (message "compile: %s" (%. ,parsed 'compile))
	   ,parsed)))

(mode! sh-mode)
