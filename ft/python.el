(lang!
 :major-mode python-mode
 :builtin-terminal t
 :lsp "jedi-language-server"

 :formatter "black -"

 :compile
 (buffer "python3 %path")
 (cwd "cd %path && python3 %buffer")

 :repl
 (buffer "python3")
 (workspace "cd %path && python3")
 (cwd "cd %path && python3"))
