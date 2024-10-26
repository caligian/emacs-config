(setq buffer-workspaces (ht))
(setq buffer-cwds (ht))

(defun buffer-get-region (buf)
  (with-current-buffer buf
    (buffer-substring-no-properties (mark) (point))))

(defun buffer-prepend (buf &rest strings)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (dolist (s strings)
	(insert s)))))

(defun buffer-append (buf &rest strings)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-max))
      (dolist (s strings)
	(insert s)))))

;; (defun buffer-set-lines (buf start-row end-row strings)
;;   (with-current-buffer buf
;;     (save-excursion
;;       (let* ((pos (buffer--resolve-line-number buf start-row end-row)))
;; 	(goto-line (car pos))
;; 	(beginning-of-line)
;; 	(set-mark (point))
;; 	(goto-line (nth 1 pos))
;; 	(end-of-line)
;; 	(kill-region (mark) (point))
;; 	(deactivate-mark)
;; 	(dolist (s strings)
;; 	  (insert (concat s "\n")))))))

;; (defun buffer-set-text (buf start-row start-col end-row end-col strings)
;;   (with-current-buffer buf
;;     (save-excursion
;;       (let* ((row-pos (buffer--resolve-line-number buf start-row end-row))
;; 	     (col-pos (buffer--resolve-column buf start-row start-col end-row end-col)))
;; 	(goto-line (car row-pos))
;; 	(goto-column (car col-pos))
;; 	(set-mark (point))
;; 	(goto-line (nth 1 row-pos))
;; 	(goto-column (nth 1 col-pos))
;; 	(deactivate-mark)
;; 	(kill-region (mark) (point))
;; 	(dolist (s strings)
;; 	  (insert (concat s "\n")))))))

(defun buffer-prepend-at-line (buf line strings)
  (with-current-buffer buf
    (save-excursion
      (goto-line line)
      (beginning-of-line)
      (dolist (s strings)
	(insert s)))))

(defun buffer-append-at-line (buf line strings)
  (with-current-buffer buf
    (save-excursion
      (goto-line line)
      (end-of-line)
      (dolist (s strings)
	(insert s)))))

(defun buffer-line-count (buf)
  (with-current-buffer buf
    (count-lines (point-min) (point-max))))

;; (defun buffer--resolve-line-number (buf start-row end-row)
;;   (let* ((lc (buffer-line-count buf))
;; 	 (end-row (if (< end-row 0)
;; 			(+ lc end-row)
;; 		      end-row))
;; 	 (start-row (if (< start-row 0)
;; 			(+ lc start-row)
;; 		      start-row))
;; 	 (end-row (if (> end-row lc)
;; 		      lc
;; 		    end-row))
;; 	 (start-row (if (> start-row lc)
;; 		      lc
;; 		    start-row)))
;;     (if (> start-row end-row)
;; 	(error (format "start:%d end:%d" start-row end-row))
;;       `(,start-row ,end-row))))

;; (defun buffer-get-lines (buf start-row end-row)
;;   (let* ((pos (buffer--resolve-line-number buf start-row end-row)))
;;     (with-current-buffer buf
;;       (save-excursion
;; 	(goto-line (nth 0 pos))
;; 	(beginning-of-line)
;; 	(set-mark (point))
;; 	(goto-line (nth 1 pos))
;; 	(end-of-line)
;; 	(let ((out (buffer-get-region buf)))
;; 	  (deactivate-mark)
;; 	  (string-split out "\n"))))))

;; (defun buffer--resolve-column (buf start-row start-col end-row end-col)
;;   (save-excursion
;;     (let* ((lines-row (buffer--resolve-line-number buf start-row end-row))
;; 	   (first-row (car lines-row))
;; 	   (last-row (nth 1 lines-row))
;; 	   (first-line (buffer-get-line buf first-row))
;; 	   (last-line (buffer-get-line buf last-row))
;; 	   (first-max-cols (length first-line))
;; 	   (last-max-cols (length last-line))
;; 	   (last-col (if (< end-col 0)
;; 			 (+ last-max-cols end-col)
;; 		       end-col))
;; 	   (first-col (if (< start-col 0)
;; 			  (+ first-max-cols start-col)
;; 			start-col))
;; 	   (first-col (if (> first-col first-max-cols)
;; 			  first-max-cols
;; 			first-col))
;; 	   (last-col (if (> last-col last-max-cols)
;; 			 last-max-cols
;; 		       last-col))
;; 	   (first-col (if (< first-col 0)
;; 			  0
;; 			first-col))
;; 	   (last-col (if (< last-col 0)
;; 			 0
;; 		       last-col)))
;;       `(,first-col ,last-col ,first-line ,last-line))))

;; ;; TODO
;; (defun buffer-get-text (buf start-row start-col end-row end-col)
;;   (with-current-buffer buf
;;       (let* ((col-pos-with-lines (buffer--resolve-column buf
;; 							 start-row
;; 							 start-col
;; 							 end-row
;; 							 end-col))
;; 	     (row-pos (buffer--resolve-line-number buf start-row end-row))
;; 	     (start-row (nth 0 row-pos))
;; 	     (end-row (nth 1 row-pos))
;; 	     (start-col (nth 0 col-pos-with-lines))
;; 	     (end-col (nth 1 col-pos-with-lines))
;; 	     (first-line (nth 2 col-pos-with-lines))
;; 	     (last-line (nth 3 col-pos-with-lines))
;; 	     (first-line (substring first-line start-col (length first-line)))
;; 	     (last-line (substring last-line 0 end-col))
;; 	     (lines (buffer-get-lines (current-buffer) start-row end-row)))
;; 	`(,first-line ,last-line))))

(defun buffer-get-line (buf linenum)
  (with-current-buffer buf
    (save-excursion
      (goto-line linenum)
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (deactivate-mark)
      (buffer-get-region buf))))

(defun buffer-create-scratch (buf)
  (let* ((buf (or buf (get-buffer-create (make-temp-name "user-temp-buffer-")))))
    (with-current-buffer buf
      (general-define-key :keymaps 'local "q" :states 'normal 'kill-buffer-and-window))
    buf))

(defun buffer-create-global-scratch ()
  (let* ((buf (find-file-noselect "~/.emacs.d/scratch")))
    (with-current-buffer buf
      (general-define-key :keymaps 'local  :states 'normal "q" 'kill-buffer-and-window))
    buf))

(defun buffer-split (buf direction)
  (with-current-buffer (current-buffer)
    (pcase direction
      ('split (split-window-below)
	      (other-window 1)
	      (switch-to-buffer buf))
      ('vsplit (split-window-right)
	       (other-window 1)
	       (switch-to-buffer buf))
      ('frame (switch-to-buffer-other-frame buf))
      ('window (switch-to-buffer-other-window buf)))))

(defun buffer2string (buf &optional start-point end-point)
  (with-current-buffer (or buf (current-buffer))
    (buffer-substring-no-properties
     (or start-point (point-min))
     (or end-point (point-max)))))

(defalias 'buffer-contents 'buffer2string)

(defun buffer-set-string (buf string)
  (let* ((buf (or buf (current-buffer))))
    (save-excursion
      (beginning-of-buffer)
      (set-mark (point))
      (end-of-buffer)
      (deactivate-mark)
      (kill-region (mark) (point))
      (insert string))))

(defun buffer-major-mode (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    major-mode))

(defun buffer-current-line-number (&optional buf)
  (let* ((buf (or buf (current-buffer)))
	 (linenum (with-current-buffer buf (line-number-at-pos))))
    linenum))

(defun buffer-current-line (&optional buf)
  (let* ((buf (or buf (current-buffer))))
   (buffer-get-line buf (buffer-current-line-number buf))))

(defun scratch-buffer-split (&optional direction)
  (let* ((direction (or direction :right))
	 (buf (buffer-create-global-scratch))
	 (win (get-buffer-window buf)))
    (when (not win)
      (with-current-buffer (current-buffer)
	(if (eq direction :right)
	    (split-window-horizontally)
	  (split-window-vertically))
	(other-window 1)
	(switch-to-buffer buf)))))

(defun scratch-buffer-below ()
  (interactive)
  (funcall #'scratch-buffer-split :below))

(defun scratch-buffer-right ()
  (interactive)
  (funcall #'scratch-buffer-split :right))

(defun buffer-replace-region (buf new-region)
  (with-current-buffer (or buf (current-buffer))
    (delete-region (mark) (point))
    (insert new-region)))

(defun buffer-delete-region (buf)
  (with-current-buffer (or buf (current-buffer))
    (delete-region (mark) (point))))
