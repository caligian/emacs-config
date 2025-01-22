(setq-default lexical-binding t)

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
(require 'eieio)
(require 'f)
(require 'dash)
(require 'ts)
(require 'ht)
(require 'smartparens-config)

(evil-mode 1)

;; basic config API
(load-file "~/.emacs.d/lisp/utils.el")
(load-file "~/.emacs.d/lisp/globals.el")

;; load all packages
(load-file "~/.emacs.d/packages.el")

;; load rest of the api
(load-file "~/.emacs.d/lisp/buffer.el")
(load-file "~/.emacs.d/lisp/modes.el")
(load-file "~/.emacs.d/lisp/repl.el")
(load-file "~/.emacs.d/lisp/compiler.el")
(load-file "~/.emacs.d/lisp/async-process.el")
(load-file "~/.emacs.d/lisp/async-formatter.el")
(load-file "~/.emacs.d/lisp/R.el")
(load-file "~/.emacs.d/config/mappings.el")
(load-file "~/.emacs.d/config/hooks.el")

;; continue setup
(mode-config-load-directory)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(set-frame-font "Liberation Mono 12" nil t)
(set-language-environment "utf-8")
(set-frame-parameter nil 'alpha-background 100)
(pixel-scroll-precision-mode t)
(global-auto-revert-mode 1)
(auto-insert-mode 1)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'ansi-term-mode 'emacs)
(evil-mode t)
(winner-mode t)
(smartparens-global-mode)
(local-config-load-hooks)
(local-config-load-mappings)
(local-config-setup-temp-buffers)

;; misc stuff
(add-hook 'find-file-hook 'auto-insert)
(add-to-list 'default-frame-alist '(alpha-background . 100))
