(setq evil-undo-system 'undo-fu)

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

(use-package kaolin-themes
  :config
  ;; (load-theme 'kaolin-aurora t)
  )

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

(use-package org-bullets
  :config
  (add-hook! org-mode-hook (org-bullets-mode t)))

(use-package org-modern
  :config
  (add-hook! org-mode-hook (org-modern-mode)))

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

  (setq
   centaur-tabs-set-icons t
   centaur-tabs-height 32
   centaur-tabs-set-bar 'under)

  (kbd!
   :states '(normal)
   :prefix "SPC t"
   "t" 'centaur-tabs--create-new-tab
   "n" 'centaur-tabs-forward
   "n" 'centaur-tabs-forward
   "p" 'centaur-tabs-backward
   "." 'centaur-tabs-counsel-switch-group
   "f" 'centaur-tabs-forward-group
   "b" 'centaur-tabs-backward-group
   "0" 'centaur-tabs-select-beg-tab
   "$" 'centaur-tabs-select-end-tab
   "q" 'centaur-tabs-kill-all-buffers-in-current-group)

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
  (recentf-mode 1)
  (setq
   recentf-save-file "~/.cache/emacs/recentf"
   recentf-max-saved-items 10000
   recentf-max-menu-items 5000)
  (run-at-time nil 300 'recentf-save-list))

(use-package treemacs
  :config
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
		treemacs-deferred-git-apply-delay        0.5
		treemacs-directory-name-transformer      #'identity
		treemacs-display-in-side-window          t
		treemacs-eldoc-display                   'simple
		treemacs-file-event-delay                2000
		treemacs-file-extension-regex            treemacs-last-period-regex-value
		treemacs-file-follow-delay               0.2
		treemacs-file-name-transformer           #'identity
		treemacs-follow-after-init               t
		treemacs-expand-after-init               t
		treemacs-find-workspace-method           'find-for-file-or-pick-first
		treemacs-git-command-pipe                ""
		treemacs-goto-tag-strategy               'refetch-index
		treemacs-header-scroll-indicators        '(nil . "^^^^^^")
		treemacs-hide-dot-git-directory          t
		treemacs-indentation                     2
		treemacs-indentation-string              " "
		treemacs-is-never-other-window           nil
		treemacs-max-git-entries                 5000
		treemacs-missing-project-action          'ask
		treemacs-move-files-by-mouse-dragging    t
		treemacs-move-forward-on-expand          nil
		treemacs-no-png-images                   nil
		treemacs-no-delete-other-windows         t
		treemacs-project-follow-cleanup          nil
		treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
		treemacs-position                        'left
		treemacs-read-string-input               'from-child-frame
		treemacs-recenter-distance               0.1
		treemacs-recenter-after-file-follow      nil
		treemacs-recenter-after-tag-follow       nil
		treemacs-recenter-after-project-jump     'always
		treemacs-recenter-after-project-expand   'on-distance
		treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
		treemacs-project-follow-into-home        nil
		treemacs-show-cursor                     nil
		treemacs-show-hidden-files               t
		treemacs-silent-filewatch                nil
		treemacs-silent-refresh                  nil
		treemacs-sorting                         'alphabetic-asc
		treemacs-select-when-already-in-treemacs 'move-back
		treemacs-space-between-root-nodes        t
		treemacs-tag-follow-cleanup              t
		treemacs-tag-follow-delay                1.5
		treemacs-text-scale                      nil
		treemacs-user-mode-line-format           nil
		treemacs-user-header-line-format         nil
		treemacs-wide-toggle-width               70
		treemacs-width                           35
		treemacs-width-increment                 1
		treemacs-width-is-initially-locked       t
		treemacs-workspace-switch-cleanup        nil)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

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

  (treemacs-hide-gitignored-files-mode nil)

  (kbd! :states 'normal
		"SPC ``" 'treemacs
		"SPC `b" 'treemacs-bookmark
		"SPC `f" 'treemacs-find-file
		"SPC `t" 'treemacs-find-tag))

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
  (setq lsp-modeline-diagnostics-scope :workspace
		lsp-headerline-breadcrumb-segments 'symbols)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :config
  (kbd! :states 'normal :prefix "SPC" "SPC" 'lsp-ivy-workspace-symbol))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package apheleia
  :config
  (kbd! :prefix "SPC" "cf" 'apheleia-format-buffer))
