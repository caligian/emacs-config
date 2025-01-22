(define-key! SPC-defaults (:prefix "SPC")
  (normal
   "bm" bookmark-set
   "br" "C-x C-q"
   "bk" delete-window
   "bq" (lambda ()
		  (interactive)
		  (kill-buffer (current-buffer)))
   "bl" "C-x <left>"
   "bh" "C-u C-SPC"
   "bn" next-buffer
   "bp" previous-buffer)
  (normal
   "fp" (lambda nil
		  (interactive)
		  (find-file-other-frame "~/.emacs.d/lisp"))
   "fs" save-buffer
   "fw" write-file
   "fF" find-file-read-only)
  (normal
   "wv" split-window-horizontally
   "ws" split-window-vertically
   "wj" windmove-down
   "wk" windmove-up
   "wl" windmove-right
   "wh" windmove-left
   "wo" delete-other-windows)
  (normal
   "qq" ":qa!"
   "qx" ":xa")
  (normal
   ",p" "\"+p"
   ",," scratch-buffer-below 
   ",;" scratch-buffer-right)
  (visual
   ",y" "\"+y")
  (normal
   "v" (general-key "C-x v"))
  (normal
   "gg" magit
   "gc" magit-commit
   "gs" magit-stage-buffer-file
   "gu" magit-unstage-buffer-file
   "gp" magit-push
   "g?" magit-status)
  (normal
   "." counsel-projectile-switch-to-buffer
   ">" counsel-projectile-switch-project
   "/" counsel-projectile-rg
   "SPC" ivy-resume)
  (normal
   :keymaps 'org-mode-map
   "mm" (general-key "C-c")
   "mv" (general-key "C-c C-v")
   "m!" org-babel-hide-result-toggle-maybe
   "mcb" org-babel-execute-buffer
   "mcc" org-babel-execute-src-block))

(define-key! toggle-evil-mode nil
  (normal
   "C-SPC" (lambda nil
			 (interactive)
			 (if (not evil-local-mode)
				 (turn-on-evil-mode)
			   (turn-off-evil-mode)))))

(define-key! completion-and-misc nil
  ((normal visual)
   "M-/" hippie-expand
   "M-SPC" company-complete-common
   "M-=" align-regexp
   "M-_" count-words-region))


(define-key! flymake nil
  (normal
   :keymaps flymake-mode-map
   "M-n" flymake-goto-next-error
   "M-p" flymake-goto-prev-error
   "SPC l?" flymake-show-buffer-diagnostics
   "SPC ld" flymake-show-project-diagnostics))
