(mode-config!
 :id python-mode
 :builtin-terminal t
 :formatter "black -"

 :compile
 (buffer "python3 %path")
 (cwd "cd %path && python3 %buffer")

 :repl
 (buffer "python3")
 (workspace "python3")
 (cwd "python3")

 :hooks
 (eglot-ensure))
