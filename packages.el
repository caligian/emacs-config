(setq use-package-always-ensure t)

(use-package doom-themes
  :defer nil
  :config
  (setq doom-themes-enable-bold t
		doom-themes-enable-italic t)
  (load-theme 'doom-henna t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package general
  :ensure t
  :defer nil) 
 
(use-package evil
  :ensure t
  :config
  (evil-mode))

(use-package projectile
  :ensure t
  :defer 50
  :config
  (projectile-mode +1))

(use-package ts
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (smartparens-mode)
  (require 'smartparens-config))

(use-package transient
  :ensure t)

(use-package ht
  :ensure t)

(use-package f
  :ensure t)

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t
  :defer 50
  :config (counsel-mode)) 

;; load use-package
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package evil-collection
  :ensure t
  :after (evil)
  :config
  (require 'evil-collection)) 

(use-package undo-fu
  :ensure t) 

(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  (global-evil-surround-mode t)) 

(use-package evil-snipe
  :ensure t
  :after (evil)
  :config
  (evil-snipe-mode t))

(use-package tree-sitter-langs
  :ensure t
  :after (tree-sitter)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode))

(use-package tree-sitter
  :ensure t
  :defer 2000
  :config
  (require 'tree-sitter))

(use-package popwin
  :ensure t
  :defer nil
  :config
  (popwin-mode)
  (dolist (pattern '("^\\*" messages-buffer-mode))
	(push pattern popwin:special-display-config)))

(use-package company-box
  :ensure t
  :config
  (add-hook 'company-mode-hook 'company-box-mode))

(use-package all-the-icons
  :ensure t
  :defer nil) 

(use-package company 
  :ensure t
  :defer nil
  :hook (find-file . global-company-mode))
  :init
  (setq company-idle-delay 0.05)

(use-package doom-modeline
  :ensure t
  :defer nil
  :config
  (doom-modeline-mode t)) 

(use-package ess
  :ensure t
  :defer 300
  :config
  (load-file "~/.emacs.d/lisp/R.el"))

(use-package yasnippet
  :ensure t
  :defer 1000
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

(use-package ligature
  :ensure t
  :defer 100
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~""~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  (global-ligature-mode t))

(use-package real-auto-save
  :ensure t
  :config
  (setq real-auto-save-interval 5)
  (require 'real-auto-save)
  (add-hook 'prog-mode-hook 'real-auto-save-mode))

(use-package outline-indent
  :ensure t
  :config
  (outline-indent-ellipsis " ▼ ")
  (outline-indent-minor-mode))

(use-package python-mode :ensure t)

(use-package pipenv
  :ensure t
  :config
  (add-hook 'python-mode-hook 'pipenv-mode)
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package mono-complete
  :ensure t
  :config
  (define-key mono-complete-mode-map
	      (kbd "<tab>")
	      'mono-complete-expand-or-fallback))

(use-package highlight-defined
  :ensure t
  :config
  (add-hook 'find-file-hook 'highlight-defined-mode))

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'find-file-hook 'global-diff-hl-mode))

(use-package rg
  :ensure t
  :defer 100)

(use-package magit
  :ensure t
  :defer 100)

(use-package treesit-auto
  :ensure t
  :after (tree-sitter)
  :config
  (global-treesit-auto-mode))

(use-package evil-escape
  :ensure t
  :after (evil)
  :config
  (evil-escape-mode)
  (global-set-key (kbd "C-c C-g") 'evil-escape))

(use-package evil-visualstar
  :ensure t
  :after (evil)
  :config
  (global-evil-visualstar-mode))

(use-package ace-window
  :ensure t
  :defer 300
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package treemacs
  :ensure t
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
  :ensure t
  :after (treemacs))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs))

(use-package treemacs-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :ensure t
  :after (magit))

(use-package eglot)

(use-package apheleia
  :ensure t
  :defer 1000)

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package auto-virtualenv
  :ensure t
  :defer 1000
  :config
  (setq auto-virtualenv-verbose t)
  (add-hook 'python-mode-hook 'auto-virtualenv-setup))

(use-package elixir-mode
  :ensure t)

(use-package mix
  :ensure t
  :hook (elixir-mode . mix-minor-mode))

(use-package web-mode
  :ensure t
  :config
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(use-package exunit
  :ensure t
  :hook (elixir-mode . exunit-mode)
  :config
  (general-define-key :keymaps 'elixir-mode-map
					  :states 'normal
					  :prefix "SPC m"
					  "t" (general-simulate-key "C-c ,")))

(use-package symbols-outline
  :after (evil eglot)
  :config
  (evil-set-initial-state 'symbols-outline-mode 'emacs)
  (general-define-key
   :states 'normal
   :prefix "SPC l"
   "s" 'symbols-outline-show)
  (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)
  (setq symbols-outline-window-position 'left)
  (symbols-outline-follow-mode))

(use-package which-key
  :defer nil
  :config
  (which-key-mode))

(use-package ivy-hydra)
