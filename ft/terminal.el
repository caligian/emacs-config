(lang!
 :major-mode terminal-mode
 :map
 (normal :prefix "SPC r"
	 "r" #'terminal-workspace-start-below
	 "s" #'terminal-workspace-below
	 "v" #'terminal-workspace-right
	 "k" #'terminal-workspace-hide
	 "q" #'terminal-workspace-kill
	 "e" #'terminal-workspace-send-line
	 "b" #'terminal-workspace-send-buffer)
 (visual :prefix "SPC r"
	 "e" #'terminal-workspace-send-region)
 (normal  :prefix "M-SPC r"
	  "r" #'terminal-buffer-start-below
	  "s" #'terminal-buffer-below
	  "v" #'terminal-buffer-right
	  "k" #'terminal-buffer-hide
	  "q" #'terminal-buffer-kill
	  "e" #'terminal-buffer-send-line
	  "b" #'terminal-buffer-send-buffer)
 (visual  :prefix "M-SPC r"
	  "e" #'terminal-buffer-send-region))