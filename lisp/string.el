(defun substr (s &optional from till)
  (let* ((len (length s))
	 (from (or from 0))
	 (till (or till len))
	 (till (if (< till 0)
		   (+ len till)
		 till))
	 (from (if (< from 0)
		   (+ len from)
		 from)))
    (substring s from till)))

(defun substr1 (s pos)
  (let* ((len (length s))
	 (pos (if (< pos 0)
		  (+ len pos)
		pos)))
    (substring s pos (+ pos 1))))

(defalias 'get-substring 'substr)
(defalias 'substring-char 'substr1)

(defun =~ (string &rest regexes)
  (cl-loop for r in regexes
	   until (not (string-match r string))
	   return t))

(defun !=~ (string &rest regexes)
 (cl-loop for r in regexes
	   until (string-match r string)
	   return t))

(defun not=~ (string &rest regexes)
 (cl-loop for r in regexes
	   until (string-match r string)
	   return t))

(defalias 'char-at/ 'substr1)
(defalias 'slice/ 'substr)

(defun strip (s)
  (replace/ s "^[ ]*" "" "[ ]*$" ""))

(defun lstip (s)
  (replace/ s "^[ ]*" ""))

(defun rstip (s)
  (replace/ s "[ ]*$" ""))

(cl-defun match/ (s re &key (start 0) (capture nil))
  (let* ((match (string-match re s (or start 0)))
		 (start (match-beginning 0))
		 (end (match-end 0)))
	(if capture
		(list start end (slice/ s start end))
	  (list start end))))

(cl-defun grep/ (s re &key (start 0) (capture t))
  (when-let* ((matches? (string-match re s (or start 0)))
			  (data (match-data 1 nil 1)))
	(cl-loop for x from 0 below (length data)
			 when (= (% x 2) 0)
			 collect (if capture
						 (when-let* ((xy (list (nth x data) (nth (1+ x) data)))
									 (matched (substr s (car xy) (cadr xy))))
						   `(,@xy ,matched))
					   (list (nth x data) (nth (1+ x) data))))))

(defun replace/ (s &rest forms)
  (let* ((final s))
    (cl-loop for i from 0 below (length forms)
	     when (= (% i 2) 0)
	     do (setq final (replace-regexp-in-string
			     (nth i forms)
			     (nth (1+ i) forms)
			     final)))
    final))

(defalias 'sed/ 'replace/)

(defalias 'split/ 'string-split)
