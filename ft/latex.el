(lang!
 :major-mode latex-mode

 :compile
 (buffer "pdflatex %buffer")
 (workspace "pdflatex %buffer")

 :hooks
 (abbrev-mode t)
 (auto-fill-mode t))

(lang!
 :major-mode LaTeX-mode

 :compile
 (buffer "pdflatex %buffer")
 (workspace "pdflatex %buffer")

 :hooks
 (abbrev-mode t)
 (auto-fill-mode t))
