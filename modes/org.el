(lang!
 :major-mode org-mode

 :hooks
 (auto-fill-mode 1)
 (org-indent-mode 1)
 (abbrev-mode 1)

 :map
 (normal :keymaps 'org-mode-map
	 :prefix "SPC c"
	 "c" #'org-latex-export-to-pdf
	 "l" #'org-latex-export-to-latex
	 "o" #'org-odt-export-to-odt))
