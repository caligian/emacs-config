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
(straight-use-package 'undo-tree)


;; when popwin does not help with evil-mode
(require 'evil)
(evil-set-initial-state 'messages-buffer-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)

(setq temp-buffer-patterns (list "\\*messages\\*" "\\*scratch\\*" "temp-buffer-*"))
(add-hook 'buffer-list-update-hook
	  (lambda nil
	    (when-let* ((buf (current-buffer))
			(bufname (buffer-name buf))
			(matches? (cl-loop for pat in temp-buffer-patterns
					   when (string-match-p pat bufname)
					   return t)))
	      (with-current-buffer buf
		(keymap-local-set (kbd "q") 'delete-window)))))


;; require all the libs
(require 'eieio)
(require 'f)
(require 'dash)
(require 'ts)
(require 'ht)
(require 'general)
(require 'undo-tree)

(defalias 'kbd! 'general-define-key)

;; basic config API
(load-file "~/.emacs.d/lisp/utils.el")

(setq LaTeX-item-indent 0)
(setq warning-minimum-level :emergency)
(setq evil-want-keybinding nil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-move-beyond-eol t)
(setq evil-undo-system 'undo-tree)
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

(evil-mode t)
(recentf-mode 1)
(winner-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(set-frame-font "Fira Code 13" nil t)
(set-language-environment "utf-8")
(set-frame-parameter nil 'alpha-background 100)
(add-to-list 'default-frame-alist '(alpha-background . 100))
(pixel-scroll-precision-mode t)
(global-auto-revert-mode 1)

(load-file "~/.emacs.d/lisp/buffer.el")
(load-file "~/.emacs.d/lisp/langs.el")
(load-file "~/.emacs.d/lisp/terminal.el")
; (load-file "~/.emacs.d/lisp/formatter.el")
(load-file "~/.emacs.d/lisp/compiler.el")
(load-file "~/.emacs.d/lisp/async-process.el")
(load-file "~/.emacs.d/lisp/async-formatter.el")
(load-file "~/.emacs.d/config/mappings.el")
(load-file "~/.emacs.d/config/hooks.el")
(load-file "~/.emacs.d/packages.el")

(lang-load-directory)
(global-undo-tree-mode t)
