(setq config-directory "~/.emacs.d")

(defun alistp (x)
  (when-let* ((test (list? x))
	      (first-value (nth 0 x)))
  (container? first-value)))

(defalias 'alist? 'alistp)

(defun basename (p)
  (let* ((ps (string-split p "/"))
	 (bname (car (last ps))))
    (if (equal bname "")
	"/"
      bname)))

(defmacro parse-arguments! (&rest args)
  (let* ((parsed (ht))
	 (pos-args '())
	 (args-len (length args))
	 (start-keyword-index (gensym))
	 (last-symbol (gensym)))
    `(progn
       (setq ,start-keyword-index
	     (cl-loop for x from 0 below ,args-len
		      when (keyword? (get@ ',args x))
		      return x))
       (setq ,last-symbol
	     (when ,start-keyword-index
	       (get@ ',args ,start-keyword-index)))
       (eachi@ ',args (lambda (i v)
			(cond
			 ((and ,start-keyword-index (< i ,start-keyword-index))
			  (setq pos-args (append@ ,pos-args v)))
			 ((keyword? v)
			  (setq ,last-symbol v)
			  (fset% ,parsed v '()))
			 (,last-symbol
			  (append% ,parsed ,last-symbol v)))))
       (list ,pos-args ,parsed))))

(defun parse-arguments (&rest args)
  (let* ((parsed (ht))
	 (pos-args '())
	 (args-len (length args))
	 (start-keyword-index
	  (cl-loop for x from 0 below args-len
		   when (keyword? (get@ args x))
		   return x))
	 (last-symbol
	  (when start-keyword-index
	    (get@ args start-keyword-index))))
    (eachi@ args (lambda (i v)
		   (cond
		    ((and start-keyword-index (< i start-keyword-index))
		     (setq pos-args (append@ pos-args v)))
		    ((keyword? v)
		     (setq last-symbol v)
		     (fset% parsed v '()))
		    (last-symbol
		     (append% parsed last-symbol v)))))
    (list pos-args parsed)))

(defun flatten-arguments (args-h &optional head-lst)
  (let* ((final-form head-lst))
    (each% args-h
	   (lambda (k v)
	     (setq final-form (append@ final-form k))
	     (setq final-form (append final-form v))))
    final-form))

(defun flatten-arguments-and-eval (args-h &optional head-lst)
  (eval (flatten-arguments args-h head-lst)))

(defun keywordp (symbol)
  (and (symbolp symbol)
       (string-match-p "^:" (symbol-name symbol))))

(defalias 'keyword? 'keywordp)
(defalias 'list? 'listp)
(defalias 'hash-table? 'hash-table-p)
(defalias 'symbol? 'symbolp)
(defalias 'string? 'stringp)
(defalias 'buffer? 'bufferp)
(defalias 'command? 'commandp)
(defalias 'function? 'functionp)
(defalias 'number? 'numberp)
(defalias 'object? 'eieio-object-p)

(defmacro add-mode-hook! (hook &rest body)
  (declare (indent 1))
  (let* ((-hook-name (gensym))
	 (form (gensym)))
    `(progn
       (let* ((,-hook-name (symbol-name ',hook))
	      (,-hook-name (concat ,-hook-name "-mode-hook"))
	      (,-hook-name (intern ,-hook-name))))
       (add-hook ,-hook-name (lambda nil ,@body)))))

(defmacro add-hook! (hook &rest body)
  (declare (indent 1))
  `(add-hook ',hook (lambda nil ,@body)))

(defmacro add-hooks! (&rest forms)
  `(cl-loop for f in ',forms
	    do (eval (append (list 'add-hook!)
			     (list (car f))
			     (cdr f)))))

(defmacro assert (form msg &optional success)
  `(if (not ,form)
       (if (list? ',msg)
	   (error ,@msg)
	 (error msg))
     (or success t)))

(load-file "~/.emacs.d/lisp/table.el")
(load-file "~/.emacs.d/lisp/container.el")
(load-file "~/.emacs.d/lisp/path.el")
(load-file "~/.emacs.d/lisp/string.el")
