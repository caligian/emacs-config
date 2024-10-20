(lang!
 :major-mode term-mode
 :map
 (normal "q" 'delete-window)
 (visual "q" 'delete-window))

(lang!
 :major-mode ansi-term-mode
 :map
 (normal "q" 'delete-window)
 (visual "q" 'delete-window))
