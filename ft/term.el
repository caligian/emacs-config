(lang!
 :major-mode term-mode
 :hooks
 (kbd!
  :keymaps 'local
  :states '(normal visual insert)
  "q" 'delete-window))
