(setq evil-undo-system 'undo-fu)

;; load use-package
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package evil-collection
  :config
  (require 'evil-collection)) 

(use-package undo-fu
  :config
  (kbd! :states '(visual normal)
	"u" 'undo-fu-only-undo
	"C-r" 'undo-fu-only-redo))

(use-package evil-surround
  :config
  (global-evil-surround-mode t)) 

(use-package evil-snipe
  :config
  (evil-snipe-mode t))

(use-package tree-sitter-langs
  :defer t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode))

(use-package tree-sitter
  :defer t
  :config
  (require 'tree-sitter))

(use-package popwin
  :config
  (push "\\*messages\\*" popwin:special-display-config)
  (push 'messages-buffer-mode popwin:special-display-config)
  (popwin-mode))

(use-package projectile
  :config
  (projectile-mode +1)
  (general-define-key
   :states 'normal
   "SPC p" #'projectile-command-map))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package kaolin-themes)

(use-package ivy
  :config
  (ivy-mode t))

(use-package counsel
  :config
  (counsel-mode 1)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "fr" 'counsel-recentf
   "hc" 'counsel-load-theme
   "hf" 'counsel-describe-function
   "hv" 'counsel-describe-variable
   "ff" 'counsel-find-file
   "bb" 'counsel-switch-buffer
   "fg" 'counsel-git))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package all-the-icons)

(use-package company 
  :init 
  (setq company-selection-wrap-around t
	company-tooltip-align-annotations t
	company-idle-delay 0.45
	company-minimum-prefix-length 3
	company-tooltip-limit 10)
  :config
  (global-company-mode t))

(use-package ess)

(use-package yasnippet-snippets)

(use-package yasnippet
  :config
  (yas-global-mode t)
  ;; (add-to-list 'company-backends 'company-yasnippet)
  (kbd! :states 'normal :prefix "SPC" "&" (general-key "C-c &")))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  (global-ligature-mode t))

(use-package which-key
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode 1))

(use-package real-auto-save
  :init
  (setq real-auto-save-interval 5)
  :config
  (require 'real-auto-save)
  (add-hook 'prog-mode-hook 'real-auto-save-mode))

(use-package auctex)

(use-package ess-plot
  :straight (ess-plot
	     :type git
	     :host github
	     :repo "DennieTeMolder/ess-plot")
  :config
  (kbd! :keymaps 'ess-r-mode
	:prefix "SPC m"
	"p" 'ess-plot-toggle))

(use-package outline-indent
  :custom
  (outline-indent-ellipsis " ▼ ")
  :config
  (outline-indent-minor-mode))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package ivy-yasnippet
  :config
  (kbd! :states 'normal :prefix "SPC &" "." 'ivy-yasnippet))

(use-package mono-complete
  :config
  (setq mono-complete-fallback-command 'tab-to-tab-stop)
  (define-key mono-complete-mode-map (kbd "<tab>") 'mono-complete-expand-or-fallback))

(use-package highlight-defined
  :config
  (highlight-defined-mode))

(use-package elisp-autofmt
  :config
  (add-hook! emacs-lisp-mode-hook (elisp-autofmt-mode)))

(use-package eros
  :config
  (add-hook! emacs-lisp-mode-hook (eros-mode)))

(use-package erlang)

(use-package org-bullets
  :config
  (add-hook! org-mode-hook (org-bullets-mode t)))

(use-package org-modern
  :config
  (add-hook! org-mode-hook (org-modern-mode)))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package ggtags
  :config
  (ggtags-mode 1)
  (kbd! :prefix "M-."
	:states 'normal
	:keymaps 'ggtags-mode-map
	"f" 'ggtags-find-file
	"o" 'ggtags-find-other-symbol
	"/" 'ggtags-grep
	"i" 'ggtags-idutils-query
	"b" 'ggtags-browse-file-as-hypertext
	"k" 'ggtags-kill-file-buffers
	"h" 'ggtags-view-tag-history
	"j" 'ggtags-visit-project-root
	"." 'ggtags-view-search-history
	"%" 'ggtags-query-replace
	"K" 'ggtags-show-definition
	"DEL"   'ggtags-delete-tags
	"SPC"   'ggtags-save-to-register))

(use-package rg
  :config
  (kbd! :states 'normal "SPC ?" 'rg-menu))

(use-package magit)

(use-package vterm
  :ensure t)

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package evil-escape
  :config
  (evil-escape-mode)
  (global-set-key (kbd "C-c C-g") 'evil-escape))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package move-text
  :config
  (move-text-default-bindings))

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

