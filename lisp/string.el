(defun substr (s &optional from till)
  (let* ((len (length s))
		 (from (or from 0))
		 (till (or till (+ from 1)))
		 (till (if (< till 0)
				   (+ len till)
				 till))
		 (from (if (< from 0)
				   (+ len from)
				 from)))
	(substring s from till)))

(defun substr (s &optional from till)
  (let* ((len (length s))
		 (fix-ind (lambda (i)
					(let* ((i (if (< i 0) (+ len i) i))
						   (i (if (> i len) (1- len) i)))
					  i)))
		 (from (or from 0))
		 (from (funcall fix-ind from))
		 (till (or till (1+ from)))
		 (till (funcall fix-ind till)))
	(substring s from till)))

(defun =~ (string &rest regexes)
  (cl-loop for r in regexes
	   until (not (string-match r string))
	   return t))

(defun !~ (string &rest regexes)
 (cl-loop for r in regexes
	   until (string-match r string)
	   return t))

(defun strip (s)
  (lstrip (rstrip s)))

(defun lstrip (s)
  (replace-regexp-in-string "^[ ]*" "" s))

(defun rstrip (s)
  (replace-regexp-in-string "[ ]*$" "" s))

(cl-defun grep (s re &key (start 0) (capture t))
  (when-let* ((matches? (string-match re s (or start 0)))
			  (data (match-data 1 nil 1)))
	(cl-loop for x from 0 below (length data)
			 when (= (% x 2) 0)
			 collect (if capture
						 (when-let* ((xy (list (nth x data) (nth (1+ x) data)))
									 (matched (substr s (car xy) (cadr xy))))
						   `(,@xy ,matched))
					   (list (nth x data) (nth (1+ x) data))))))

(defun sed (s &rest forms)
  (let* ((final s))
    (cl-loop for i from 0 below (length forms)
			 when (= (% i 2) 0)
			 do (setq final (replace-regexp-in-string
							 (nth i forms)
							 (nth (1+ i) forms)
							 final)))
    final))
