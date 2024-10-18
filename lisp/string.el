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

(defun not=~ (string &rest regexes)
 (cl-loop for r in regexes
	   until (string-match r string)
	   return t))
