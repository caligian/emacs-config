(require 'projectile)

;; repl definition
;;	help
;;  
;;  input-filter

;; formatter definition 
;; FORM: (name args) where args is a a form or string

;; project definition:
;; FORM: (name ...rest)
;;	name
;;	root-dir
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

(class mode-config
  mode
  (workspace (list ".git"))
  (workspace-check-depth 4)
  compile-buffer
  project formatter
  builtin-terminal
  hooks mappings hook map
  repl)

(defun mode-config-add-projects (conf)
  (cl-loop for x in (%. conf 'project)
		   do (let* ((x (cdr (from-plist% x)))
					 (project-type (car x))
					 (marker-files (%. x 'root-dir))
					 (args (as-plist% (reject% x (list :name :root-dir))))
					 (form (list 'projectile-register-project-type project-type marker-files))
					 (args (append form args)))
				(eval args))))

(defun mode-config-define-keys (conf)
  (let* ((map (%. conf 'map))
		 (mappings (%. conf 'mappings)))
	(cl-loop for x in mappings
			 do (let* ((state (car x))
					   (args (cdr x))
					   (args (append `(:states ,state :keymaps ,map))))
				  (apply 'general-define-key args)))))

(defun mode-config-add-hooks (conf)
  (funcall 'add-hook
		   (%. conf 'hook)
		   (eval (append (list 'lambda nil) (%. conf 'hooks)))))

(defun mode-config-add-formatters (conf)
  (cl-loop for x in (%. conf 'formatter)
		   do (let* ((defined (%. apheleia-mode-alist (%. conf mode)))
					 (name (add-to-list defined (car x)))
					 (args (cdr x)))
				(push (list name args) apheleia-formatters))))

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
			(,repl (from-alist% (%. ,parsed 'repl)))
			(,ws (or (%. ,parsed 'workspace) (list ".git")))
			(,ws-check-depth (or (%. ,parsed 'workspace-check-depth) 4))
			(,repl-input-filter-forms (or (%. ,repl 'input-filter) 's))
			(,repl-input-filter (append '(lambda (s it)) (->list ,repl-input-filter-forms)))
			(,repl-input-filter (eval ,repl-input-filter))
			(,repl-help (or (%. ,repl 'help) (lambda (s) (format "help(%s)" s)))))

	   (%! ,parsed 'hook ,hook)
	   (%! ,parsed 'map ,map)
	   (%! ,parsed 'repl ,repl)
	   (%! ,repl 'input-filter ,repl-input-filter)
	   (%! ,repl 'help ,repl-help)

	   (mode-config-add-projects ,parsed)
	   (mode-config-add-formatters ,parsed)
	   (mode-config-define-keys ,parsed)
	   (mode-config-add-hooks ,parsed)

	   (local-config-set :modes ',m ,parsed)

	   (when (%. ,parsed 'builtin-terminal)
		 (add-hook ,hook 'repl-mode)
		 (repl-create-mappings))

	   ,parsed)))

(mode! sh-mode)
