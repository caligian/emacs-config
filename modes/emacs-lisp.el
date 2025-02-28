(defun describe-symbol-at-point ()
  (interactive)
  (when-let* ((fn (thing-at-point 'symbol t)))
	(describe-variable (intern fn))))


(mode! emacs-lisp-mode
  :mappings
  (normal :prefix "SPC e"
		  "b" 'eval-buffer
		  "e" 'eval-last-sexp
		  ":" 'eval-expression
		  "d" 'eval-defun)
  (visual :prefix "SPC e"
		  "e" 'eval-region)
  (normal "K" 'describe-symbol-at-point))
