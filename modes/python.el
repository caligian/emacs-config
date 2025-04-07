(mode! python-mode
  :builtin-terminal t
  :repl
  (use-input-file "load -y %s")
  (command "ipython3")

  :compile
  (default ("python3" file)))
