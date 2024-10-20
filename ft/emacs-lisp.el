(lang!
 :major-mode emacs-lisp-mode
 :map
 (normal  :prefix "SPC e"
	  "b" 'eval-buffer
	  "e" 'eval-last-sexp
	  ":" 'eval-expression
	  "d" 'eval-defun)

 (visual :prefix "SPC e"
	 "e" 'eval-region)

 (normal "SPC k" (lambda ()
		   (interactive)
		   (when-let* ((fn (thing-at-point 'symbol t)))
		     (describe-variable (intern fn))))
	 "K" (lambda ()
	       (interactive)
	       (when-let* ((fn (thing-at-point 'symbol t)))
		 (describe-function (intern fn)))))
 )
