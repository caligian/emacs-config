(define-key! misc (:prefix "SPC")
  (normal
   "hf" 'describe-function
   "hv" 'describe-variable
   "hc" 'counsel-load-theme
   "SPC" (defun pop-local-mark ()
		   (interactive)
		   (set-mark-command -1))
   ",p" "\"+p"
   ",," 'scratch-buffer-below 
   ",;" 'scratch-buffer-right)
  (visual
   ",y" "\"+y"))

(define-key! exit (:prefix "SPC")
  (normal
   "qq" ":qa!"
   "qx" ":xa"))

(define-key! editing ()
  ((normal insert)
   "M-y" 'counsel-yank-pop
   "C-<backspace>" (general-simulate-key "C-u <backspace>")))

(define-key! file (:prefix "SPC")
  (normal
   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fp" (lambda nil
		  (interactive)
		  (find-file-other-frame "~/.emacs.d/lisp"))
   "fs" 'save-buffer
   "fw" 'write-file
   "fF" 'find-file-read-only))

(define-key! buffer (:prefix "SPC")
  (normal
   "bl" (defun switch-to-last-buffer ()
		  (interactive)
		  (let* ((prev (cget :last-buffer))
				 (cur (current-buffer))
				 (prev (or prev cur)))
			(cset :last-buffer cur)
			(when (not (eq cur prev))
			  (switch-to-buffer prev))))
   "bb" 'counsel-switch-buffer
   "bm" 'bookmark-set
   "bB" 'counsel-bookmark
   "br" "C-x C-q"
   "bk" 'delete-window
   "bq" (defun kill-current-buffer ()
		  (interactive)
		  (kill-buffer (current-buffer)))
   "bn" 'next-buffer
   "bp" 'previous-buffer))

(define-key! window (:prefix "SPC")
  (normal
   "wv" 'split-window-horizontally
   "ws" 'split-window-vertically
   "wj" 'windmove-down
   "wk" 'windmove-up
   "wl" 'windmove-right
   "wh" 'windmove-left
   "wo" 'delete-other-windows))

(define-key! vc (:prefix "SPC")
  (normal
   "v" (general-key "C-x v")))

(define-key! magit (:prefix "SPC")
  (normal
   "gg" 'magit
   "gc" 'magit-commit
   "gs" 'magit-stage-buffer-file
   "gu" 'magit-unstage-buffer-file
   "gp" 'magit-push
   "g?" 'magit-status))

(define-key! org (:prefix "SPC")
  (normal
   :keymaps 'org-mode-map
   "e" (general-key "C-c C-e")
   "m" (general-key "C-c")
   "v" (general-key "C-c C-v")
   "ck" 'org-babel-hide-result-toggle-maybe
   "cb" 'org-babel-execute-buffer
   "cc" 'org-babel-execute-src-block))

(define-key! projectile (:prefix "SPC")
  (normal
   "p" 'projectile-command-map))

(define-key! completion-and-misc nil
  ((normal visual insert emacs)
   "M-/" 'hippie-expand
   "M-SPC" 'company-complete-common
   "M-=" 'align-regexp
   "M-_" 'count-words-region))

(define-key! flymake nil
  (normal
   :keymaps 'flymake-mode-map
   "M-n" 'flymake-goto-next-error
   "M-p" 'flymake-goto-prev-error)
  (normal
   :prefix "SPC l"
   "F" 'flymake-mode
   "d" 'flymake-show-project-diagnostics))

(define-key! undo-fu nil
  ((visual normal)
   "u" 'undo-fu-only-undo
   "C-r" 'undo-fu-only-redo))

(define-key! yasnippet nil
  (normal
   "SPC &" (general-key "C-c &")))

(define-key! formatter (:prefix "SPC")
  (normal
   "cf" 'apheleia-format-buffer))

(define-key! treemacs (:prefix "SPC")
  (normal
   "t" 'treemacs))

(define-key! swiper ()
  ((normal visual)
   "C-s" 'swiper))

(define-key! toggle-bg (:prefix "SPC h")
  (normal
   "C" (defun toggle-theme-bg ()
		 (interactive)
		 (if-let* ((current (car custom-enabled-themes))
				   (current-name (symbol-name current)))
			 (if (=~ current-name "light")
				 (load-theme (or (cget :dark-theme) 'doom-henna) t)
			   (load-theme (or (cget :light-theme) 'doom-acario-light) t))
		   (load-theme (cget :light-theme) t)))))

(define-key! ripgrep (:prefix "SPC")
  (normal
   "?" 'rg-menu))
