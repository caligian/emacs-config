(lang!
 :major-mode python-mode
 :lsp "jedi-language-server"
 :formatter "cat %buffer | autopep8 -"
 :compile
 ((buffer "python3 %path")
  (cwd "cd %path && python3 %buffer"))
 :repl
 ((buffer "python3")
  (workspace "cd %path && python3")
  (cwd "cd %path && python3")))
