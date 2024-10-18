;; load use-package
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package evil-collection
  :config
  (require 'evil-collection))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-snipe
  :config
  (evil-snipe-mode t))

(use-package tree-sitter-langs
  :config
  (require 'tree-sitter-langs))

(use-package tree-sitter
  :config
  (require 'tree-sitter)
  (global-tree-sitter-mode))

(use-package popwin
  :config
  (popwin-mode))

(use-package magit
  :config
  (general-define-key
   :states 'normal
   :prefix "M-g"
   "g" #'magit))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

  :config
  (general-define-key
   :states 'normal
   :prefix "C-,"
   "k"    #'treemacs-delete-other-windows
   ","    #'treemacs
   "d"    #'treemacs-select-directory
   "B"    #'treemacs-bookmark
   "C-t"  #'treemacs-find-file
   "M-t"  #'treemacs-find-tag)

  :bind
  (:map global-map ("M-0" . treemacs-select-window)))

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

(use-package treemacs-persp
  :after (treemacs persp-mode)
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package projectile
  :config
  (projectile-mode +1)
  (general-define-key
   :states 'normal
   "SPC p" #'projectile-command-map))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t)
  (kaolin-treemacs-theme))     

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
   "bb" 'counsel-switch-buffer))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package all-the-icons)

(use-package treemacs-evil)

(use-package treemacs-projectile)

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
  )

(use-package mood-line
  :config
  (mood-line-mode 1))

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
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  (global-ligature-mode t))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package real-auto-save
  :init
  (setq real-auto-save-interval 5)
  :config
  (require 'real-auto-save)
  (add-hook 'prog-mode-hook 'real-auto-save-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp))))

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
