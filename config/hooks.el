(add-hook 'prog-mode-hook
	  (lambda nil
	    (abbrev-mode t)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda nil
	    (setq lexical-binding t)))

(add-hook 'LateX-mode-hook
	  (lambda nil
	    (auto-fill-mode t)
	    (abbrev-mode t)))

(add-hook 'org-mode-hook
	  (lambda nil
	    (auto-fill-mode 1)
	    (org-indent-mode 1)
	    (abbrev-mode 1)))
