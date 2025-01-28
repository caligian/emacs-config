;; load use-package
(straight-use-package 'project)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(use-package project)

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

(use-package undo-fu)

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
  (dolist (pattern '("^\\*" messages-buffer-mode))
	(push pattern popwin:special-display-config))
  (popwin-mode))

(use-package projectile
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package kaolin-themes)

(use-package ivy
  :config
  (ivy-mode t))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package all-the-icons)

(use-package company 
  :config
  (global-company-mode t))

(use-package ess)

(use-package yasnippet-snippets)

(use-package yasnippet
  :config
  (yas-global-mode t))

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

(use-package real-auto-save
  :init
  (setq real-auto-save-interval 5)
  :config
  (require 'real-auto-save)
  (add-hook 'prog-mode-hook 'real-auto-save-mode))

(use-package auctex)

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

(use-package ivy-yasnippet)

(use-package mono-complete
  :config
  (define-key mono-complete-mode-map (kbd "<tab>") 'mono-complete-expand-or-fallback))

(use-package highlight-defined
  :config
  (highlight-defined-mode))

(use-package eros
  :config
  (add-hook 'emacs-lisp-mode-hook 'eros-mode))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-modern
  :config
  (add-hook 'org-mode-hook 'org-modern-mode))

(use-package diff-hl
  :config
  (global-diff-hl-mode))


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

;; (use-package gruvbox-theme
;;   :config
;;   (load-theme 'gruvbox-dark-medium t))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package centaur-tabs
  :config
  (defun centaur-tabs-hide-tab (x)
	"Do no to show buffer X in tabs."
	(let ((name (format "%s" x)))
	  (or
	   ;; Current window is not dedicated window.
	   (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
	   (=~ name "[*]")
       (=~ name "[*]epc")
       (=~ name "[*]helm")
       (=~ name "[*]Helm")
       (=~ name "[*]Compile-Log[*]")
       (=~ name "[*]lsp")
       (=~ name "[*]company")
       (=~ name "[*]Flycheck")
       (=~ name "[*]tramp")
       (=~ name "[*]Mini")
       (=~ name "[*]help")
       (=~ name "[*]straight")
       (=~ name "[*]temp")
       (=~ name "[*]Help")
       (=~ name "[*]mybuf")
       (=~ name "^async-process")
       (=~ name "^async-formatter")
       (=~ name "[*]ansi")
       (=~ name "[*]Messages")

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
			(not (file-name-extension name))))))

  (centaur-tabs-mode 1)
  (centaur-tabs-group-by-projectile-project)

  (define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
  (define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward))

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark-high-contrast t))

(straight-use-package '(evil-ts :type git :host github :repo "foxfriday/evil-ts"))

(use-package evil-ts-obj
  :straight (evil-ts-obj :type git :host github :repo "dvzubarev/evil-ts-obj")
  :defer t
  :config
  (add-hook 'python-ts-mode-hook #'evil-ts-obj-mode))

(use-package recentf
  :config
  (recentf-mode 1))

(use-package treemacs
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package which-key
  :config
  (which-key-mode 1))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package apheleia)
