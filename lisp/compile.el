;; compile form
;; (mode!
;;  ...
;;  :compile
;;  ("run" ("python" file))
;;  ("test" ("pytest" file)))

(cl-defun compile-buffer (&optional buf (direction "below"))
  (interactive)
  (when-let* ((buf (or buf (current-buffer)))
			  (bufname (buffer-file-name buf))
			  (mm (buffer-major-mode buf))
			  (ws (or (find-buffer-workspace buf) (dirname buf)))
			  (path-alist (append@ (cget :path-lookup-alist)
								   `(file ,bufname)
								   `(buffer ,bufname)
								   `(cwd ,ws)))
			  (commands (cget :modes mm 'compile))
			  (lst (cl-loop for command in commands
							collect (list (car command)
										  (expression (last@ command) path-alist))))
			  (on-exit (eval
						`(lambda (-proc)
						   (let* ((stderr (%. -proc 'stderr))
								  (stdout (%. -proc 'stdout))
								  (stderr-not-empty (not (empty-p stderr)))
								  (stdout-not-empty (not (empty-p stdout)))
								  (contents (cond
											 ((and stderr-not-empty
												   stdout-not-empty)
											  `("STDERR:" ,@stderr "" "STDOUT:" ,@stdout))
											 (stdout-not-empty
											  `("STDOUT:" ,@stdout))
											 (stderr-not-empty
											  `("STDERR:" ,@stderr)))))
							 (make-temp-buffer
							  :read-only t
							  :split ,direction
							  :contents contents)))))
			  (on-input-with-edit (eval
								   `(lambda (args)
									  (async-process-start
									   (async-process-init
										:name (concat "compilation-" ,bufname)
										:command (last@ (plist-get args :lines))
										:on-exit ,on-exit))
									  (message "running command: %s" text)))))
	(message "%s" on-input-with-edit)
	(menu@ "Compile actions % " lst
		   :action `(1
					 ("e"
					  (lambda (selection)
						(with-current-buffer
							(make-temp-buffer
							 :split ,direction
							 :contents '("Edit command and press <CR>")
							 :on-input ,on-input-with-edit)
						  (insert selection)))
					  "edit-and-compile")
					 ("o"
					  (lambda (selection)
						(message "running command directly: %s" selection)))))))

(setq test-py-buf (find-file-noselect "test.py"))
(setq a-proc (compile-buffer test-py-buf))
(%. a-proc 'command)
a-proc


(get-text-property 0 'value (propertize "helloworld" 'value (cdr
															 (list 1 2 3))))


(async-process-start
 (async-process-init :command "ls -l"
					 :on-success (lambda (proc)
								   (make-temp-buffer
									:split "below"
									:contents (string-join (%. proc 'stdout) "\n")))))

(defun my-action-1 (x)
  (message "action-1: %s" x))

(defun my-action-2 (x)
  (message "action-2: %s" x))

(defun my-action-3 (x)
  (message "action-3: %s" x))

(defun my-command-with-3-actions ()
  (interactive)
  (ivy-read "test: " '("foo" "bar" "baz")
            :action '(1
                      ("o" (lambda (x) (message x))  "action 1")
                      ("j" my-action-2 "action 2")
                      ("k" my-action-3 "action 3"))))

(my-command-with-3-actions)

(menu@ "test: " '(("a" "foo") ("b" "bar") ("c" "baz"))
       :action '(1
                 ("o" (lambda (x) (message x))  "action 1")
                 ("j" my-action-2 "action 2")
                 ("k" my-action-3 "action 3")))
