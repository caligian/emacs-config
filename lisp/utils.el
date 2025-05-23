(setq config-directory "~/.emacs.d") 
(setq system-types (list 'integer 'number 'float
			 'symbol
			 'string 'array 'cons 'list
			 'marker
			 'overlay
			 'window-configuration
			 'process
			 'window
			 'subr
			 'compiled-function
			 'buffer
			 'char-type
			 'vector 'bool-vector
			 'hash-table
			 'font-spec 'font-object 'font-entity))

(defun assert (test &rest format-args)
  (unless test
    (funcall 'error format-args))
  t)

(defmacro typecheck (let-forms &rest body)
  `(progn
     (cl-loop for binding in ',let-forms
	      do (let* ((type (car binding))
			(obj (cadr binding))
			(obj-type (type-of obj)))
		   (unless (eq obj-type type)
		     (error "expected [%s], got [%s] %s" type obj-type obj))))
     ,@body))

(defun assert-type (type obj)
  (let* ((obj-type (type-of obj)))
    (unless (eq obj-type obj)
      (error "expected [%s], got [%s] %s" type obj-type obj))
    t))

(defun alistp (x)
  (and (listp x) (listp (car x))))

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
	   (cl-loop for k in (keys% ,parsed)
				do (let* ((v (%. ,parsed k)))
					 (when (and (list? v)
								(= 1 (length v))
								(not (alist? v)))
					   (set% ,parsed k (car v)))))
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
(defalias 'process-live? 'process-live-p)

(defmacro assert (form msg &optional success)
  `(if (not ,form)
       (if (list? ',msg)
	   (error ,@msg)
	 (error ,msg))
     (or success t)))

(defmacro progn-lambda (&rest forms)
  `(lambda nil ,@forms))

(defmacro let-lambda (args &rest forms)
  (declare (indent 1))
  (cl-loop for a in args
	   when (or (not (list? a))
		    (not (length= a 2)))
	   do (error "expected form (VAR . VALUE), got %s" a))
  `(lambda nil
     (let* ,args
       ,@forms)))

(defmacro let-lambda* (args forms)
  (declare (indent 1))
  `(let-lambda ,args ,@forms))

(defun partial-apply (fn &rest outer-args)
  (lambda (&rest inner-args)
	(apply fn (append outer-args inner-args))))

(defun rpartial-apply (fn &rest outer-args)
  (lambda (&rest inner-args)
	(apply fn (append inner-args outer-args))))

(defmacro class (name parents &rest attribs)
  (declare (indent 2))
  (cl-with-gensyms (final-form)
	`(let* ((,final-form
			 (cl-loop for var in ',attribs
					  collect (if (list? var)
								  `(,(car var)
									:initarg ,(intern (concat ":" (symbol-name (car var))))
									:initform ,(cadr var))
								`(,var
								  :initarg ,(intern (concat ":" (symbol-name var)))
								  :initform nil))))
			(,final-form (append@ (list 'defclass ',name ',parents) ,final-form)))
	   (eval ,final-form))))

(defun expression (form exp)
  (cl-loop for x in form
		   collect (if (list? x)
					   (expression x exp)
					 (if-let* ((expansion (%. exp x)))
						 expansion
					   x))))

(defmacro substitute (form exp)
  `(expression ',form ',exp)) 

(defun closure->lambda (fn)
  (when (closurep fn)
	(eval `(lambda ,@(nthcdr 2 fn)))))

(defmacro deflambda (args &rest body)
  (declare (indent 1))
  `(closure->lambda (lambda ,args ,@body)))

(defun lambdap (fn)
  (and (listp fn)
	   (functionp fn)
	   (eq (car fn) 'lambda) fn))

(defalias 'lambda? 'lambdap)
