(mode-config!
 :id python-mode
 :builtin-terminal t
 :formatter "black -"

 :compile
 (buffer "ipython %path")
 (cwd "cd %path && ipython %buffer")

 :repl
 (buffer "ipython")
 (workspace "ipython")
 (cwd "ipython")

 :repl-use-input-file
 "load -y %s"

 :hooks
 (lsp))
