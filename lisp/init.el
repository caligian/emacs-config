(add-hook 'emacs-lisp-mode-hook
	  (lambda nil
	    (setq lexical-binding t)))

(setq-default lexical-binding t)
(setq straight-use-package-by-default t)

;; NECESSARY
(straight-use-package 'evil)
(straight-use-package 'undo-tree)
(straight-use-package 's)
(straight-use-package 'dash)
(straight-use-package 'ht)
(straight-use-package 'f)
(straight-use-package 'ts)
(straight-use-package 'general)
(straight-use-package 'use-package)

(defalias 'kbd! 'general-define-key)

(require 'eieio)
(require 'f)
(require 'dash)
(require 'ts)
(require 'ht)
(require 'general)

;; basic config API
(load-file "~/.emacs.d/lisp/utils.el")
(load-file "~/.emacs.d/lisp/table.el")
(load-file "~/.emacs.d/lisp/container.el")
(load-file "~/.emacs.d/lisp/path.el")
(load-file "~/.emacs.d/lisp/string.el")

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

(global-undo-tree-mode)
(evil-mode t)
(recentf-mode 1)
(winner-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(set-frame-font "Fira Code 13" nil t)
(set-language-environment "utf-8")
(set-frame-parameter nil 'alpha-background 96)
(add-to-list 'default-frame-alist '(alpha-background . 96))
(pixel-scroll-precision-mode t)
(global-auto-revert-mode 1)

(load-file "~/.emacs.d/lisp/buffer.el")
(load-file "~/.emacs.d/lisp/langs.el")
(load-file "~/.emacs.d/lisp/terminal.el")
(load-file "~/.emacs.d/lisp/formatter.el")
(load-file "~/.emacs.d/lisp/packages.el")
(load-file "~/.emacs.d/lisp/pkgs.el")
(load-file "~/.emacs.d/lisp/compiler.el")
(load-file "~/.emacs.d/config/mappings.el")
(load-file "~/.emacs.d/config/hooks.el")

(lang-load-directory)
