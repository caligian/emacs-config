(general-define-key
 :prefix "SPC b"
 :states '(normal)
 "m" 'bookmark-set
 "f" 'formatter-format-buffer
 "r" "C-x C-q"
 "k" 'delete-window
 "q" (lambda ()
       (interactive)
       (kill-buffer (current-buffer)))
 "l" "C-x <left>"
 "h" "C-u C-SPC"
 "n" #'next-buffer
 "p" #'previous-buffer)

(general-define-key
 :prefix "SPC f"
 :states '(normal)
 "p" (lambda nil
       (interactive)
       (find-file-other-frame "~/.emacs.d/lisp"))
 "s" 'save-buffer
 "w" 'write-file
 "F" 'find-file-read-only
 "k" 'kill-buffer)

(general-define-key
 :prefix "SPC w"
 :states '(normal)
 "v" 'split-window-horizontally
 "s" 'split-window-vertically
 "j" 'windmove-down
 "k" 'windmove-up
 "l" 'windmove-right
 "h" 'windmove-left
 "o" 'delete-other-windows)

(general-define-key
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

(general-define-key
 :prefix "SPC e"
 :states '(normal)
 "b" 'eval-buffer
 "e" 'eval-last-sexp
 ":" 'eval-expression
 "d" 'eval-defun)

(general-define-key
 :prefix "SPC e"
 :states '(visual)
 "e" 'eval-region)

(general-define-key
 :prefix "SPC q"
 :states 'normal
 "q" ":qa!"
 "x" ":xa")

(general-define-key
 :prefix "SPC ,"
 :states 'normal
 "p" "\"+p"
 "," #'scratch-buffer-below 
 ";" #'scratch-buffer-right) 

(general-define-key
 :prefix "SPC ,"
 :states 'visual
 "y" "\"+y")

(general-define-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 "SPC k" (lambda ()
	   (interactive)
	   (when-let* ((fn (thing-at-point 'symbol t)))
	     (describe-variable (intern fn))))
 "K" (lambda ()
       (interactive)
       (when-let* ((fn (thing-at-point 'symbol t)))
	 (describe-function (intern fn)))))

(general-define-key
 :keymaps 'term-mode-map
 :states 'normal
 "q" (lambda nil
       (interactive)
       (delete-window  (get-buffer-window (current-buffer)))))

(general-define-key
 :keymaps 'global
 :states 'normal
 :prefix "SPC x"
 "x" (lambda ()
       (interactive)
       (terminal-shell-start :below))
 "s" #'terminal-shell-split
 "v" (lambda nil
       (interactive)
       (terminal-shell-split :right))
 "k" #'terminal-shell-hide
 "q" #'terminal-shell-kill
 "e" #'terminal-shell-send-line
 "b" #'terminal-shell-send-buffer)

(general-define-key
 :keymaps 'global
 :states 'visual
 :prefix "SPC x"
 "e" #'terminal-shell-send-region)

(general-define-key
 :keymaps 'global
 :states 'normal
 :prefix "SPC r"
 "r" #'terminal-workspace-start-below
 "s" #'terminal-workspace-below
 "v" #'terminal-workspace-right
 "k" #'terminal-workspace-hide
 "q" #'terminal-workspace-kill
 "e" #'terminal-workspace-send-line
 "b" #'terminal-workspace-send-buffer)

(general-define-key
 :keymaps 'global
 :states 'visual
 :prefix "SPC r"
 "e" #'terminal-workspace-send-region)

(general-define-key
 :keymaps 'global
 :states 'normal
 :prefix "M-SPC r"
 "r" #'terminal-buffer-start-below
 "s" #'terminal-buffer-below
 "v" #'terminal-buffer-right
 "k" #'terminal-buffer-hide
 "q" #'terminal-buffer-kill
 "e" #'terminal-buffer-send-line
 "b" #'terminal-buffer-send-buffer)

(general-define-key
 :keymaps 'global
 :states 'visual
 :prefix "M-SPC r"
 "e" #'terminal-buffer-send-region)

(general-define-key
 :states 'normal
 :prefix "SPC c"
 "c" #'compiler-compile-workspace)

(general-define-key
 :states 'normal
 :prefix "M-SPC c"
 "c" #'compiler-compile-buffer)
