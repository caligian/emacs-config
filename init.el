(add-hook 'emacs-lisp-mode-hook
	  (lambda nil
	    (setq lexical-binding t)))

(setq-default lexical-binding t)
(setq straight-use-package-by-default t)

;; NECESSARY
(straight-use-package 'evil)
(straight-use-package 's)
(straight-use-package 'dash)
(straight-use-package 'ht)
(straight-use-package 'f)
(straight-use-package 'ts)
(straight-use-package 'general)
(straight-use-package 'smartparens)

;; when popwin does not help with evil-mode
(require 'evil)
(require 'general)

;; (evil-set-initial-state 'messages-buffer-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'ansi-term-mode 'emacs)

(setq temp-buffer-patterns (list "\\*ansi-term"
								 "\\*messages\\*"
								 "\\*scratch\\*"
								 "temp-buffer-*"))

(defun temp-buffer-map-q ()
  (when-let* ((buf (current-buffer))
			  (bufname (buffer-name buf))
			  (has-local-map? (keymapp evil-normal-state-local-map))
			  (matches? (cl-loop for pat in temp-buffer-patterns
								 when (string-match-p pat bufname)
								 return t)))
	(with-current-buffer buf
	  (define-key evil-normal-state-local-map (kbd "q") 'delete-window))))

(add-hook 'buffer-list-update-hook 'temp-buffer-map-q)

;; require all the libs
(require 'eieio)
(require 'f)
(require 'dash)
(require 'ts)
(require 'ht)
(require 'smartparens-config)

(smartparens-global-mode)

(defalias 'kbd! 'general-define-key)

;; basic config API
(load-file "~/.emacs.d/lisp/utils.el")

(setq LaTeX-item-indent 0)
(setq warning-minimum-level :emergency)
(setq evil-want-keybinding nil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-move-beyond-eol t)
(setq evil-undo-system 'undo-fu)
(setq recentf-max-menu-items 25)
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
(setq-default tab-width 4)
(setq-default tab-stop-list nil)

(evil-mode t)
(recentf-mode 1)
(winner-mode t)

(when (window-system)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(global-display-line-numbers-mode)
(set-frame-font "Liberation Mono 12" nil t)
(set-language-environment "utf-8")
(set-frame-parameter nil 'alpha-background 95)
(add-to-list 'default-frame-alist '(alpha-background . 95))
(pixel-scroll-precision-mode t)
(global-auto-revert-mode 1)
(auto-insert-mode 1)

(setq
   auto-insert-directory "~/.emacs.d/templates"
   auto-insert-alist '((ess-r-mode . "template.R")))

(add-hook 'find-file-hook 'auto-insert)

;; load api
(load-file "~/.emacs.d/lisp/buffer.el")
(load-file "~/.emacs.d/lisp/modes.el")
(load-file "~/.emacs.d/lisp/repl.el")
(load-file "~/.emacs.d/lisp/compiler.el")
(load-file "~/.emacs.d/lisp/async-process.el")
(load-file "~/.emacs.d/lisp/async-formatter.el")
(load-file "~/.emacs.d/lisp/R.el")
(load-file "~/.emacs.d/config/mappings.el")
(load-file "~/.emacs.d/config/hooks.el")
(load-file "~/.emacs.d/packages.el")

(mode-config-load-directory)
(setq indent-tabs-mode nil)
(setq-default tab-width 4)

;; some org stuff
(setq org-babel-load-languages
	  (append org-babel-load-languages '((R . t) (python . t))))

(add-hook! org-mode-hook
  (org-babel-do-load-languages '((R . t))))


;; (global-undo-tree-mode t)

(run-at-time (current-time) 300 'recentf-save-list)
