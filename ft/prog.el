(lang!
 :major-mode prog-mode

 :hooks
 (abbrev-mode t)
 (formatter! :buffer (current-buffer))

 :map
 (visual :prefix "SPC b" "f" #'format-region!)
 (normal :prefix "SPC b" "f" #'format-buffer!))
