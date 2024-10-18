(lang!
 :major-mode ess-r-mode
 :compile ((buffer "R %buffer"))
 :repl ((buffer "R")
	(workspace "cd %path && R")
	(cwd "cd %path && R"))
 :map ((normal
	:prefix "SPC e"
	"f" 'ess-load-file
	"d" 'ess-eval-function
	"e" 'ess-eval-line
	"b" 'ess-eval-buffer
	"D" 'ess-eval-function-and-go
	"E" 'ess-eval-line-and-go
	"B" 'ess-eval-buffer-and-go)
       (normal
	"K" 'ess-display-help-on-object
	"C-n" 'ess-complete-object-name)
       (normal
	:prefix "SPC r"
	"s" 'ess-switch-to-inferior-or-script-buffer
	"r" 'R)
       (visual
	:prefix "SPC e"
	"e" 'ess-eval-region
	"E" 'ess-eval-region-and-go)
       (normal
	:prefix "SPC m"
	"i" 'ess-install-library))
 :hooks (abbrev-mode t))

(kbd!
 :keymaps 'inferior-ess-r-mode-map
 :states 'normal
 "q" 'delete-window)

