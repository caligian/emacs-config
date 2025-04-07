(setq local-config
	  (ht
	   (:light-theme 'doom-acario-light)
	   (:dark-theme 'doom-henna)
	   (:diary-buffer nil)
	   (:diary-path "~/diary.org")
	   (:shell-command "/bin/bash")
	   (:temp-buffer-patterns '("temp-*" "^async-" "^[*]" "Flymake diagnostics"))
	   (:terminal-shell-command "/usr/bin/zsh")
	   (:repls (ht))
	   (:modes (ht))
	   (:processes (ht))
	   (:buffer-workspaces (ht))
	   (:workspace-buffers (ht))
	   (:mappings (ht))
	   (:hooks (ht))
	   (:R-processes (ht))
	   (:R-workspace-processes (ht))
	   (:path-lookup-alist '((HOME (getenv "HOME"))
							 (ROOT  "/")
							 (config "~/.emacs.d/")
							 (state "~/.local/state")
							 (xdg-config "~/.config")
							 (bashrc "~/.bashrc")
							 (zshrc "~/.zshrc")
							 (shell "/usr/bin/zsh")))))

(defun local-config-get (&rest ks-and-value)
  (apply '%. local-config ks-and-value))

(defun local-config-set (var &rest ks-and-value)
  (apply 'fset% local-config var ks-and-value))

(defmacro local-config-define-key (id overrides &rest forms)
  (declare (indent 2))
  (cl-with-gensyms (real-id final-forms)
	`(let* ((,real-id ',id)
			(,final-forms
			 (cl-loop for form in ',forms
					  do (let* ((state (car form))
									 (rest-args (append ',overrides (cdr form)))
									 (out (append `(:states ',state) rest-args)))
								(eval (append (list 'general-define-key) out)))))))))

(defmacro local-config-add-hook (id hook &rest forms)
  (declare (indent 2))
  (cl-with-gensyms (form final-form)
	`(let* ((,form (lambda nil ,@forms))
			(,final-form (list ',hook ,form)))
	   (apply 'add-hook ,final-form))))

(defun local-config-setup-temp-buffers ()
  (cl-loop for regex in (%. local-config :temp-buffer-patterns)
		   do (push (list regex :regexp t) popwin:special-display-config)))

(defun local-config-load-files ()
  (cl-loop for file in (list-emacs-lisp-files "~/.emacs.d/config" t)
		   do (load-file file)))

(defun local-config-create-path (words &optional overrides)
  (let* ((lookup (cget :path-lookup-alist))
		 (lookup (append lookup overrides))
		 (words (cl-loop for word in words collect
						 (let* ((exp (cond
									  ((listp word) (eval word))
									  ((stringp word) word)
									  (t (%. lookup word)))))
						   (if exp
							   (if (listp exp) (eval exp) exp) 
							 (symbol-name word))))))
	(string-join words "/")))

(defmacro local-config-create-path! (words &optional overrides)
  "if `overrides' is a symbol, it must point to an alist and not to another symbol"
  (local-config-create-path words overrides))

(defalias 'define-key! 'local-config-define-key)
(defalias 'add-hook! 'local-config-add-hook)
(defalias 'create-path 'local-config-create-path)
(defalias 'create-path! 'local-config-create-path)
(defalias 'cget 'local-config-get)
(defalias 'cset 'local-config-set)
