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
