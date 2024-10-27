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

(kbd!
 :states 'visual
 :prefix "SPC x"
 "e" #'terminal-shell-send-region)

(kbd!
 :states 'normal
 :prefix "SPC"
 "v" (general-key "C-x v"))

(kbd!
 :states '(normal insert visual)
 "M-SPC" (general-simulate-key "<escape>SPC"))

(kbd!
 :states 'normal
 :prefix "SPC"
 "gg" 'magit
 "gs" 'magit-stage-buffer-file
 "gu" 'magit-unstage-buffer-file
 "gp" 'magit-push
 "g?" 'magit-status)
