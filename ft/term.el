(lang!
 :major-mode term-mode
 :hooks
 (kbd!
  :keymaps 'local
  :states '(normal visual)
  "q" 'delete-window))
