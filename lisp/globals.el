(defalias 'kbd! 'general-define-key)

(setq-default lexical-binding t)

(setq straight-use-package-by-default t)

(setq LaTeX-item-indent 0)

(setq warning-minimum-level :emergency)

(setq evil-want-keybinding nil
	  evil-want-integration t
	  evil-want-keybinding nil
	  evil-move-beyond-eol t
	  evil-undo-system 'undo-fu)

(setq display-line-numbers-type 'relative)

(setq inhibit-startup-screen t)

(setq select-enable-clipboard nil)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-themes-treemacs-enable-variable-pitch nil)

(setq org-element-use-cache nil)

(setq x-alt-keysym 'meta)

(setq backup-by-copying t      
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backup")))

(setq undo-tree-history-directory-alist (list '(".*" . "~/.emacs.d/cache")))

(setq-default tab-width 4
			  tab-stop-list nil)

(setq auto-insert-directory "~/.emacs.d/templates"
	  auto-insert-alist '((ess-r-mode . "template.R")))

(setq indent-tabs-mode nil)

(setq org-babel-load-languages '((R . t) (python . t)))

(setq company-selection-wrap-around t
	  company-tooltip-align-annotations t
	  company-idle-delay 0.45
	  company-minimum-prefix-length 3
	  company-tooltip-limit 10)

(setq lsp-modeline-diagnostics-scope :workspace
	  lsp-headerline-breadcrumb-segments 'symbols
	  lsp-modeline-diagnostics-enable t
	  lsp-headerline-breadcrumb-enable t)

(setq recentf-save-file "~/.cache/emacs/recentf"
	  recentf-max-saved-items 10000
	  recentf-max-menu-items 5000)

(setq mono-complete-fallback-command 'tab-to-tab-stop)

(setq centaur-tabs-set-icons t
	  centaur-tabs-height 32
	  centaur-tabs-set-bar 'under)

(setq treemacs-deferred-git-apply-delay        0.5
	  treemacs-directory-name-transformer      #'identity
	  treemacs-display-in-side-window          t
	  treemacs-eldoc-display                   'simple
	  treemacs-file-event-delay                2000
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
