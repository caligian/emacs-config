(setq local-config
	  (ht
	   (:shell-command "/usr/bin/zsh")
	   (:temp-buffer-patterns '("^\\*" "temp-*" "^async-"))
	   (:terminal-shell-command "/usr/bin/zsh")
	   (:repls (ht))
	   (:modes (ht))
	   (:processes (ht))
	   (:buffer-workspaces (ht))
	   (:workspace-buffers (ht))
	   (:mappings (ht))
	   (:hooks (ht))))

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
					  collect (let* ((state (car form))
									 (rest-args (append ',overrides (cdr form)))
									 (out (append `(:states ',state) rest-args)))
								(eval (append (list 'general-define-key) out))
								out))))
	   (local-config-set :mappings ,real-id ,final-forms))))

(defmacro local-config-add-hook (id hook &rest forms)
  (declare (indent 2))
  (cl-with-gensyms (form final-form)
	`(let* ((,form (lambda nil ,@forms))
			(,final-form (list ',hook ,form)))
	   (apply 'add-hook ,final-form)
	   (local-config-set :hooks ',id ,final-form))))

(defun local-config-setup-temp-buffers ()
  (cl-loop for regex in (%. local-config :temp-buffer-patterns)
		   do (push (list regex :regexp t) popwin:special-display-config)))

(defalias 'define-key! 'local-config-define-key)
(defalias 'add-hook! 'local-config-add-hook)
