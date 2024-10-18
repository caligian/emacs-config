(setq config-directory "~/.emacs.d")

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

(defalias '%. 'slot-value)
(defalias '%.? 'slot-exists-p)

(defmacro add-hook! (&rest forms)
  (let* ((HOOK (gensym))
	 (BODY (gensym))
	 (FN (gensym)))
    `(dolist (f ',forms)
       (setq ,HOOK (car f))
       (setq ,BODY (cdr f))
       (setq ,FN (append `(lambda nil) ,BODY))
       (add-hook ,HOOK ,FN))))

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
		     (message "%s" v)
		     (setq last-symbol v)
		     (fset% parsed v '()))
		    (last-symbol
		     (append% parsed last-symbol v)))))
      (list pos-args parsed)))

(defun keywordp (symbol)
  (and (symbolp symbol)
       (string-match-p "^:" (symbol-name symbol))))

(defalias 'keyword? 'keywordp)
(defalias 'list? 'listp)
(defalias 'hash-table? 'hash-table-p)
(defalias 'symbol? 'symbolp)
