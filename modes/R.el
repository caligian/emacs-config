(mode-config!
 :id ess-r-mode
 :compile
 (buffer "R %buffer")
 (workspace "cd %path && R %buffer")
 (cwd "cd %path && R %buffer")

 :map
 (normal :prefix "SPC r"
		 "r" R-start
		 "e" R-eval-line
		 "b" R-eval-buffer
		 "E" R-eval-region
		 "k" R-hide
		 "s" R-split-below
		 "v" R-split-right)

 (visual :prefix "SPC r"
		 "e" R-eval-region)

 :hooks
 (setq-local ess-indent-level 2
			 ess-indent-offset 2
			 ess-ask-for-ess-directory t
			 ess-eval-visibly t
			 ess-use-tracebug t)
 (setq-local tab-width 2)
 (abbrev-mode t)

 :project
 (default
  (".git" "lib")
  :run "R main.R"))
