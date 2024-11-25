(mode-config!
 :id org-mode

 :hooks
 (auto-fill-mode 1)
 (org-indent-mode 1)
 (abbrev-mode 1)

 :map
 (normal :prefix "SPC c"
	 "c" org-latex-export-to-pdf
	 "l" org-latex-export-to-latex
	 "o" org-odt-export-to-odt)
 (normal "TAB" org-cycle))
