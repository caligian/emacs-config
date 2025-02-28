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

(use-package kaolin-themes)

(use-package ivy)

(use-package counsel)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package all-the-icons)

(use-package company 
  :config
  (global-company-mode t))

(use-package mood-line
  :config
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

(use-package ess)

(use-package yasnippet-snippets)

(use-package yasnippet
  :config
  (yas-global-mode t))

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
  (define-key! ripgrep (:prefix "SPC")
	(normal
	 "?" 'rg-menu)))

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
	   (=~ name "[*]temp-buffer")
	   (=~ name "temp-buffer")
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
  :init
  (setq lsp-enable-links nil)

  :hook ((lsp-mode . lsp-enable-which-key-integration)
		 (python-mode . lsp))

  :custom 
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)

  :config
  (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package apheleia)

(use-package lsp-jedi
  :ensure t)

(use-package ultra-scroll
  :straight
  (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll")

  :init
  (setq scroll-conservatively 101
		scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; Enable vertico
(use-package vertico
  :config
  (vertico-mode)
  (keymap-set vertico-map "?" #'minibuffer-completion-help)
  (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
  (keymap-set vertico-map "M-TAB" #'minibuffer-complete))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :custom
  (setq scroll-preserve-screen-position t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  :init
  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args
                (lambda (args)
                  (cons (format "[CRM%s] %s"
                                (string-replace "[ \t]*" "" crm-separator)
                                (car args))
                        (cdr args))))))

(use-package marginalia
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         
   ("C-;" . embark-dwim)        
   ("C-h B" . embark-bindings)) 

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :bind (("M-X" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s /" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)

         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch

         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element


  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package auto-virtualenv
  :config
  (setq auto-virtualenv-verbose t)
  (auto-virtualenv-setup))

(use-package beacon
  :config
  (beacon-mode 1))

;; use-package with package.el:
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
