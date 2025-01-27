(define-key! misc (:prefix "SPC")
  (normal
   ",p" "\"+p"
   ",," 'scratch-buffer-below 
   ",;" 'scratch-buffer-right)
  (visual
   ",y" "\"+y"))

(define-key! exit (:prefix "SPC")
  (normal
   "qq" ":qa!"
   "qx" ":xa"))

(define-key! file (:prefix "SPC")
  (normal
   "fp" (lambda nil
		  (interactive)
		  (find-file-other-frame "~/.emacs.d/lisp"))
   "fs" 'save-buffer
   "fw" 'write-file
   "fF" 'find-file-read-only))

(define-key! buffer (:prefix "SPC")
  (normal
   "bm" 'bookmark-set
   "br" "C-x C-q"
   "bk" 'delete-window
   "bq" (lambda ()
		  (interactive)
		  (kill-buffer (current-buffer)))
   "bl" "C-x <left>"
   "bh" "C-u C-SPC"
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
   "mm" (general-key "C-c")
   "mv" (general-key "C-c C-v")
   "m!" 'org-babel-hide-result-toggle-maybe
   "mcb" 'org-babel-execute-buffer
   "mcc" 'org-babel-execute-src-block))

(define-key! ivy (:prefix "SPC")
  (normal
   "." 'counsel-projectile-switch-to-buffer
   ">" 'counsel-projectile-switch-project
   "/" 'counsel-projectile-rg
   "SPC" 'ivy-resume)
  (normal
   "fr" 'counsel-recentf
   "hc" 'counsel-load-theme
   "hf" 'counsel-describe-function
   "hv" 'counsel-describe-variable
   "ff" 'counsel-find-file
   "bb" 'counsel-switch-buffer
   "fg" 'counsel-git
   "&." 'ivy-yasnippet))

(define-key! projectile (:prefix "SPC")
  (normal
   "p" 'projectile-command-map))

(define-key! toggle-evil-mode nil
  (normal
   "C-SPC" (lambda nil
			 (interactive)
			 (if (not evil-local-mode)
				 (turn-on-evil-mode)
			   (turn-off-evil-mode)))))

(define-key! completion-and-misc nil
  ((normal visual)
   "M-/" 'hippie-expand
   "M-SPC" 'company-complete-common
   "M-=" 'align-regexp
   "M-_" 'count-words-region))

(define-key! flymake nil
  (normal
   :keymaps 'flymake-mode-map
   "M-n" 'flymake-goto-next-error
   "M-p" 'flymake-goto-prev-error
   "SPC l?" 'flymake-show-buffer-diagnostics
   "SPC ld" 'flymake-show-project-diagnostics))

(define-key! undo-fu nil
  ((visual normal)
   "u" 'undo-fu-only-undo
   "C-r" 'undo-fu-only-redo))

(define-key! yasnippet nil
  (normal
   "SPC &" (general-key "C-c &")))

(define-key! formatter (:prefix "SPC")
  (normal
   "cf" 'apheleia))

(define-key! lsp (:prefix "SPC")
  (normal
   "ls" 'lsp-ivy-workspace-symbol))

(define-key! centaur-tabs (:prefix "SPC t")
  (normal
   "t" 'centaur-tabs--create-new-tab
   "n" 'centaur-tabs-forward
   "n" 'centaur-tabs-forward
   "p" 'centaur-tabs-backward
   "." 'centaur-tabs-counsel-switch-group
   "f" 'centaur-tabs-forward-group
   "b" 'centaur-tabs-backward-group
   "0" 'centaur-tabs-select-beg-tab
   "$" 'centaur-tabs-select-end-tab
   "q" 'centaur-tabs-kill-all-buffers-in-current-group))

(define-key! treemacs (:prefix "SPC")
  (normal
   "``" 'treemacs
   "`b" 'treemacs-bookmark
   "`f" 'treemacs-find-file
   "`t" 'treemacs-find-tag))
