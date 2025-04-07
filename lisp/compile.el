;; compile form
;; (mode!
;;  ...
;;  :compile
;;  ("run" ("python" file))
;;  ("test" ("pytest" file)))

(defun compile-buffer--on-exit (direction)
  (eval `(lambda (-proc)
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

(defun compile-buffer--start-process (bufname command direction)
  (async-process-start
   (async-process-init
	:name (concat "compilation-" bufname)
	:command command
	:on-exit (compile-buffer--on-exit direction))))

(defun compile-buffer--on-input (bufname direction)
  (eval `(lambda (args)
		   (let* ((lines (reverse (plist-get args :lines)))
				  (command (cl-loop for l in lines
									when (=~ l "[a-zA-Z0-9]")
									return l)))
			 (compile-buffer--start-process ,bufname command ,direction)))))

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
							collect (let* ((entry (car command))
										   (command (last@ command))
										   (expanded-command (expression command path-alist)))
									  (list entry (string-join expanded-command " "))))))
	(menu@ (format "[%s] Compile actions %% " mm)
		   lst
		   :action `(2
					 ("e"
					  (lambda (selection)
						(with-current-buffer
							(make-temp-buffer
							 :split ,direction
							 :contents '("Edit command and press <CR>")
							 :on-input (compile-buffer--on-input ,bufname ,direction))
						  (insert selection)))
					  "edit-and-compile")
					 ("o"
					  (lambda (selection)
						(compile-buffer--start-process
						 ,bufname selection ,direction)))))))
