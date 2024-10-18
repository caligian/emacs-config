(setq config-directory "~/.emacs.d")

(defalias '%. 'slot-value)
(defalias '%.? 'slot-exists-p)

(defun basename (p)
  (let* ((ps (string-split p "/"))
	 (bname (car (last ps))))
    (if (equal bname "")
	"/"
      bname)))

(defun set-instance-attributes (obj &rest kvs)
  (cl-loop for i from 0 below (length kvs)
	   when (= (% i 2) 0)
	   do (setf (slot-value obj (nth i kvs))
		    (nth (+ i 1) kvs))))

(defmacro %! (obj &rest kvs)
  (let* ((obj (if (listp obj) (eval obj) obj)))
  `(progn
     (apply #'set-instance-attributes ,obj ',kvs)
     ,obj)))

(defmacro add-hook! (&rest forms)
  (let* ((HOOK (gensym))
	 (BODY (gensym))
	 (FN (gensym)))
    `(dolist (f ',forms)
       (setq ,HOOK (car f))
       (setq ,BODY (cdr f))
       (setq ,FN (append `(lambda nil) ,BODY))
       (add-hook ,HOOK ,FN))))

(defmacro parse-arguments! (&rest args)
  (let* ((parsed (hash-table%))
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
  (let* ((parsed (hash-table%))
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
