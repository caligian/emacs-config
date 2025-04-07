(mode! org-mode
 :hooks
 (auto-fill-mode 1)
 (org-indent-mode 1)
 (abbrev-mode 1)
 (org-toggle-link-display)

 :mappings
 (normal "TAB" 'org-cycle))
