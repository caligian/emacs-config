(mode! ess-r-mode
  :compile (command " " path) 

  :mappings
  (normal :prefix "SPC <return>"
		  "<return>" 'R-root-start
		  "k" 'R-root-hide
		  "q" 'R-root-stop
		  "s" 'R-root-split
		  "v" 'R-root-split-right)

  (normal :prefix "SPC m"
		  "i" 'R-install
		  "d" 'R-document
		  "I" 'R-document-and-install
		  "D" 'R-load-devtools)

  (normal :prefix "SPC r"
		  "R" 'R-restart
		  "r" 'R-start
		  "e" 'R-eval-line
		  "." 'R-eval-paragraph
		  "f" 'R-eval-function
		  "b" 'R-eval-buffer
		  "E" 'R-eval-region
		  "k" 'R-hide
		  "s" 'R-split-below
		  "q" 'R-quit
		  "v" 'R-split-right)

  (normal :prefix "g"
		  "f" 'R-goto-next-function
		  "b" 'R-goto-previous-function
		  "n" 'R-goto-next-local
		  "p" 'R-goto-previous-local
		  "F" 'R-mark-function
		  "." 'R-mark-paragraph)

  (visual :prefix "SPC r"
		  "e" 'R-eval-region)

  :hooks
  (setq ess-ask-for-ess-directory nil
		ess-eval-visibly t
		ess-use-tracebug t)
  (setq-local tab-width 2
			  ess-indent-level 2
			  ess-indent-offset 2)
  (abbrev-mode t))

