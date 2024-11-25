(mode-config!
 :id latex-mode

 :compile
 (buffer "pdflatex %buffer")
 (workspace "pdflatex %buffer")

 :hooks
 (abbrev-mode t)
 (auto-fill-mode t))

(mode-config!
 :id LaTeX-mode

 :compile
 (buffer "pdflatex %buffer")
 (workspace "pdflatex %buffer")

 :hooks
 (abbrev-mode t)
 (auto-fill-mode t))
