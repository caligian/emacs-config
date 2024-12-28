(kbd!
 :prefix "SPC b"
 :states '(normal)
 "m" 'bookmark-set
 "r" "C-x C-q"
 "k" 'delete-window
 "q" (lambda ()
       (interactive)
       (kill-buffer (current-buffer)))
 "l" "C-x <left>"
 "h" "C-u C-SPC"
 "n" #'next-buffer
 "p" #'previous-buffer)

(kbd!
 :prefix "SPC f"
 :states '(normal)
 "p" (lambda nil
       (interactive)
       (find-file-other-frame "~/.emacs.d/lisp"))
 "s" 'save-buffer
 "w" 'write-file
 "F" 'find-file-read-only
 "k" 'kill-buffer)

(kbd!
 :prefix "SPC w"
 :states '(normal)
 "v" 'split-window-horizontally
 "s" 'split-window-vertically
 "j" 'windmove-down
 "k" 'windmove-up
 "l" 'windmove-right
 "h" 'windmove-left
 "o" 'delete-other-windows)

(kbd!
 :states 'normal
 :prefix "SPC t"
 "t" 'tab-new
 "b" 'switch-to-buffer-other-tab
 "d" 'dired-other-tab
 "k" 'tab-close
 "K" 'tab-bar-close-other-tabs
 "n" 'tab-next
 "p" 'tab-previous
 "." 'tab-switch
 "r" 'tab-rename
 "P" 'tab-bar-history-back
 "N" 'tab-bar-history-forward
 "f" 'find-file-other-tab
 "F" 'find-file-read-only-other-tab)

(kbd!
 :prefix "SPC q"
 :states 'normal
 "q" ":qa!"
 "x" ":xa")

(kbd!
 :prefix "SPC ,"
 :states 'normal
 "p" "\"+p"
 "," #'scratch-buffer-below 
 ";" #'scratch-buffer-right) 

(kbd!
 :prefix "SPC ,"
 :states 'visual
 "y" "\"+y")

(kbd!
 :states 'normal
 :prefix "SPC"
 "v" (general-key "C-x v"))

(kbd!
 "C-SPC" (lambda nil
	   (interactive)
	   (if (not evil-local-mode)
	       (turn-on-evil-mode)
	     (turn-off-evil-mode))))

(kbd!
 :states 'normal
 :prefix "SPC"
 "gg" 'magit
 "gc" 'magit-commit
 "gs" 'magit-stage-buffer-file
 "gu" 'magit-unstage-buffer-file
 "gp" 'magit-push
 "g?" 'magit-status)

(kbd!
 "M-/" 'hippie-expand
 "M-SPC" 'company-complete-common
 "M-=" 'align-regexp
 "M-_" 'count-words-region)

(kbd!
 :states 'normal
 :prefix "SPC"
 "." 'counsel-projectile-switch-to-buffer
 ">" 'counsel-projectile-switch-project
 "/" 'counsel-projectile-rg
 "'" 'ivy-resume)

(kbd! :states '(normal)
      :keymaps 'flymake-mode-map
      "M-n" 'flymake-goto-next-error
      "M-p" 'flymake-goto-prev-error
      "SPC l?" 'flymake-show-buffer-diagnostics
      "SPC ld" 'flymake-show-project-diagnostics
      "SPC lf" 'eglot-format-buffer)

(kbd! :keymaps 'term-mode-map
      "C-w C-h" 'windmove-left
      "C-w C-l" 'windmove-right
      "C-w C-j" 'windmove-down
      "C-w C-k" 'windmove-up)

(kbd! :prefix "SPC"
	  :states '(normal)
	  :keymaps 'org-mode-map
	  "m" (general-key "C-c")
	  "v" (general-key "C-c C-v")
	  "!" 'org-babel-hide-result-toggle-maybe
	  "cb" 'org-babel-execute-buffer
	  "cc" 'org-babel-execute-src-block)
