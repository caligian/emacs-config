(setq kill-buffer-query-functions nil)
(setq repl-shell-command "/usr/bin/zsh")
(setq repl-mode-map (make-sparse-keymap))

(defgroup repl ()
  "Convenient global terminal"
  :prefix "repl-")

(define-minor-mode repl-mode
  "Toggle repl-mode"
  :init-value nil
  :lighter (" REPL")
  :keymap repl-mode-map
  :group 'repl)

(defclass user-repl ()
  ((process :initarg :process
	    :initform nil)
   (process-buffer :initarg :process-buffer
		   :initform nil)
   (working-dir :initarg :working-dir)
   (type :initarg :type
	 :initform :buffer)
   (command :initarg :command
	    :initform nil)
   (mode-config :initarg :mode-config)))

;; id <buffer> <repl> OR
;; id workspace <repl> OR
;; id cwd <repl> 
(setq repls (ht))
(setq repl-shell (user-repl :working-dir "/home/skeletor"
			    :type :shell
			    :command repl-shell-command))


(cl-defmethod repl-live? ((it user-repl))
  (process-live-p (%. it 'process)))

(cl-defmethod repl-start ((it user-repl))
  (when (not (repl-live? it))
    (with-slots (command path type working-dir) it
      (split-window-vertically)
      (other-window 1)
      (let* ((proc-buf (ansi-term repl-shell-command))
	     (proc (get-buffer-process proc-buf)))
	(%setq it
	       'process proc
	       'process-buffer proc-buf)
	(process-send-string proc (concat "cd " working-dir "\n"))
	(when command
	  (process-send-string proc (concat command "\n")))
	(delete-window)
	it))))

(cl-defmethod repl-split ((it user-repl) &optional direction)
  (when (and (repl-live? it) (not (repl-visible? it)))
    (with-current-buffer (current-buffer)
      (pcase (or direction :below)
	(:below (split-window-below))
	(:right (split-window-right)))
      (with-slots (process-buffer) it
	(other-window 1)
	(switch-to-buffer process-buffer)))))

(cl-defmethod repl-split-right ((it user-repl))
  (repl-split it :right))

(cl-defmethod repl-stop ((it user-repl))
  (with-slots (process process-buffer) it
    (when (repl-live? it)
      (repl-hide it)
      (kill-process process)
      (kill-buffer process-buffer))))

(cl-defmethod repl-kill ((it user-repl))
  (repl-stop it))

(cl-defmethod repl-visible? ((it user-repl))
  (with-slots (process-buffer) it
    (get-buffer-window process-buffer)))

(cl-defmethod repl-hide ((it user-repl))
  (if-let* ((window (get-buffer-window (%. it 'process-buffer))))
      (delete-window window)))

(cl-defmethod repl-send-string ((it user-repl) s)
  (when (repl-live? it)
    (process-send-string (%. it 'process)
			 (concat s "\n"))))

(defmacro repl--create-shell-functions ()
  `(dolist (name '("start" "stop" "hide" "stop" "split" "split-right"))
     (eval (append@ (list 'defun (intern (concat "repl-shell-" name)) nil '(interactive))
		    (list (intern (concat "repl-" name)) repl-shell)))))

(defun repl-shell-send-region ()
  (interactive)
  (when (repl-live? repl-shell)
    (with-slots (process) repl-shell
      (with-current-buffer (current-buffer)
	(when (mark)
	  (process-send-string
	   process
	   (concat (buffer-substring-no-properties (mark) (point)) "\n")))))))

(defun repl-shell-send-line ()
  (interactive)
  (when (repl-live? repl-shell)
    (with-slots (process) repl-shell
      (with-current-buffer (current-buffer)
	(process-send-string
	 process
	 (concat (buffer-substring-no-properties
		  (line-beginning-position)
		  (line-end-position))
		 "\n"))))))

(defun repl-shell-send-buffer ()
  (interactive)
  (when (repl-live? repl-shell)
    (with-slots (process) repl-shell
      (with-current-buffer (current-buffer)
	(process-send-string
	 process
	 (concat (buffer-substring-no-properties
		  (point-min)
		  (point-max))
		 "\n"))))))


(repl--create-shell-functions)

(defun repl-find-buffer-repl (buf type &optional allow-stopped)
  (when-let* ((path (pcase type
		      ('workspace (find-buffer-workspace buf))
		      ('cwd (dirname buf))
		      (_ buf)))
	      (it (%. repls path))
	      (it (if allow-stopped
		      it
		    (when (repl-live? it)
		      it))))
    it))

(defun repl-buffer-init (buf type &optional command)
  (when-let* ((buf (or buf (current-buffer)))
	      (type (or type 'buffer))
	      (mm (buffer-major-mode buf))
	      (working-dir (pcase type
			     ('workspace (find-buffer-workspace buf))
			     (_ (dirname buf))))
	      (config (%. mode-configs mm))
	      (command (or command (%. config 'repl type)))
	      (x (user-repl :type type
			    :command command
			    :working-dir working-dir
			    :mode-config config)))
    (pcase type
      ('buffer
       (%! repls buf x))
      (_
       (%! repls working-dir x)))
    x))

(defmacro repl-live-dispatch (BUF TYPE &rest forms)
  (declare (indent 2))
  `(when-let* ((it (repl-find-buffer-repl ,BUF ',TYPE)))
     (declare (ignore it))
     (funcall (lambda (buf type) ,@forms) ,BUF ',TYPE)))

(defmacro repl-dispatch (BUF TYPE &rest forms)
  (setq fn (gensym))
  (declare (indent 2))
  `(when-let* ((it (or (repl-find-buffer-repl ,BUF ',TYPE t)
		       (repl-buffer-init ,BUF ',TYPE)))
	       (buf ,BUF)
	       (type ',TYPE))
     (declare (ignore it))
     ,@forms))

(defun repl-buffer-send-region (buf type)
  (when-let* ((it (repl-find-buffer-repl buf type)))
    (with-slots (process) it
      (with-current-buffer buf
	(when (mark)
	  (process-send-string
	   process
	   (concat (buffer-substring-no-properties (mark) (point)) "\n")))))))

(defun repl-buffer-send-line (buf type)
  (when-let* ((it (repl-find-buffer-repl buf type)))
    (with-slots (process) it
      (with-current-buffer buf
	(process-send-string
	 process
	 (concat (buffer-substring-no-properties
		  (line-beginning-position)
		  (line-end-position))
		 "\n"))))))

(defun repl-buffer-send-buffer (buf type)
  (when-let* ((it (repl-find-buffer-repl buf type)))
    (with-slots (process) it
	(with-current-buffer buf
	  (process-send-string
	   process
	   (concat (buffer-substring-no-properties
		    (point-min)
		    (point-max))
		   "\n"))))))

(defmacro repl--dispatch-send-string-mapping-function (type name)
  (declare (indent 3))
  (let* ((fn-name (concat "repl-mapping-" (symbol-name type) "-" (symbol-name name)))
	 (fn-name (intern fn-name))
	 (dispatcher (intern (concat "repl-buffer-" (symbol-name name))))
	 (final-form nil))
    `(progn
       (setq final-form
	     (list 'defun ',fn-name '()
		   '(interactive)
		   (list ',dispatcher '(current-buffer) '',type)))
       (eval final-form))))

(defmacro repl--dispatch-mapping-function (type name live? body)
  (declare (indent 3))
  (let* ((fn-name (concat "repl-mapping-" (symbol-name type) "-" (symbol-name name)))
	 (fn-name (intern fn-name))
	 (dispatcher (if live? 'repl-live-dispatch 'repl-dispatch))
	 (final-form nil))
    `(progn
       (setq final-form
	     (list 'defun ',fn-name '()
		   '(interactive)
		   (list ',dispatcher '(current-buffer) ',type ',body)))
       (eval final-form))))

(defmacro repl--create-mapping-functions ()
  (setq fn-name (gensym))
  (setq form (gensym))
  (setq fn-names '(split split-right stop hide))
  (setq live? (gensym))
  (setq send-string? (gensym))
  `(progn
     (cl-loop for x in '(buffer workspace cwd)
	      do (progn
		   (eval (append@ '(repl--dispatch-mapping-function)
				  x 'start nil
				  '(repl-start it)))
		   (cl-loop for y in '(send-line send-region send-buffer)
			    do (eval (append@ '(repl--dispatch-send-string-mapping-function) x y)))
		   (cl-loop for y in ',fn-names
			    do (let* ((,fn-name (symbol-name y))
				      (,fn-name (concat "repl-" (symbol-name y)))
				      (,fn-name (intern ,fn-name))
				      (,form '(repl--dispatch-mapping-function))
				      (,form (append@ ,form x y t))
				      (,form (append@ ,form (list ,fn-name 'it))))
				 (eval ,form)))))))

(repl--create-mapping-functions)

(general-define-key
 :states 'normal :prefix "SPC x"
 "x" 'repl-shell-start
 "s" 'repl-shell-split
 "v" 'repl-shell-split-right
 "e" 'repl-shell-send-line
 "b" 'repl-shell-send-buffer
 "k" 'repl-shell-hide
 "q" 'repl-shell-stop)

(general-define-key
 :states 'normal :prefix "SPC mr"
 "r" 'repl-mapping-workspace-start
 "s" 'repl-mapping-workspace-split
 "v" 'repl-mapping-workspace-split-right
 "e" 'repl-mapping-workspace-send-line
 "b" 'repl-mapping-workspace-send-buffer
 "k" 'repl-mapping-workspace-hide
 "q" 'repl-mapping-workspace-stop)

(general-define-key
 :states 'normal :prefix "SPC r"
 "r" 'repl-mapping-buffer-start
 "s" 'repl-mapping-buffer-split
 "v" 'repl-mapping-buffer-split-right
 "e" 'repl-mapping-buffer-send-line
 "b" 'repl-mapping-buffer-send-buffer
 "k" 'repl-mapping-buffer-hide
 "q" 'repl-mapping-buffer-stop)

(general-define-key
 :states 'visual
 "SPC xe" 'repl-shell-send-region
 "SPC mre" 'repl-mapping-workspace-send-region
 "SPC re" 'repl-mapping-buffer-send-region)


;; (setq buf (find-file-noselect "/home/skeletor/Work/mum/o/list_progress.py"))
;; (with-current-buffer buf
;;   (repl-mapping-buffer-split-right))
