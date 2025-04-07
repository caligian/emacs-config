(package-initialize)

(setq-default lexical-binding t)

(defun turn-on-lexical-binding ()
  (setq-local lexical-binding t)) 

(turn-on-lexical-binding)
(add-hook 'emacs-lisp-mode-hook 'turn-on-lexical-binding) 

;; font stuff
(add-to-list 'default-frame-alist '(font . "RobotoMono Nerd Font Md 11"))
(set-frame-font "RobotoMono Nerd Font Md 11")
(set-language-environment "utf-8")

;; load all packages
(load-file "~/.emacs.d/packages.el")

(require 'evil)
(require 'general)
(require 'eieio)
(require 'f)
(require 'dash)
(require 'ts)
(require 'ht)

;; basic config API
(load-file "~/.emacs.d/lisp/utils.el") 
(load-file "~/.emacs.d/lisp/table.el") 
(load-file "~/.emacs.d/lisp/container.el") 
(load-file "~/.emacs.d/lisp/string.el") 
(load-file "~/.emacs.d/globals.el") 
(load-file "~/.emacs.d/lisp/config.el")
(load-file "~/.emacs.d/lisp/buffer.el")
(load-file "~/.emacs.d/lisp/path.el") 
(load-file "~/.emacs.d/lisp/modes.el")
(load-file "~/.emacs.d/lisp/repl.el")
(load-file "~/.emacs.d/lisp/diary.el")
(load-file "~/.emacs.d/config/mappings.el")
(load-file "~/.emacs.d/config/hooks.el")

(global-display-line-numbers-mode)
(pixel-scroll-precision-mode t)
(auto-insert-mode 1)
(add-hook 'find-file-hook 'auto-insert)

(when (window-system)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (recentf-mode 1))

(defun local-config-initialize ()
  (mode-config-load-directory)
  (set-frame-parameter nil 'alpha-background 100)
  (global-auto-revert-mode 1)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'ansi-term-mode 'emacs)
  (winner-mode t)
  (smartparens-global-mode)
  (local-config-load-files)
  (local-config-setup-temp-buffers)
  (add-to-list 'default-frame-alist '(alpha-background . 100))
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))

(run-with-idle-timer 0.1 nil 'local-config-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ivy-hydra which-key web-mode mix treemacs-icons-dired python-mode exunit elixir-mode all-the-icons apheleia auto-virtualenv company-box counsel diff-hl doom-modeline doom-themes dumb-jump ess evil-collection evil-escape evil-snipe evil-surround evil-visualstar exec-path-from-shell general highlight-defined ligature lsp-jedi lsp-treemacs lsp-ui mono-complete outline-indent pipenv popwin real-auto-save rg smartparens symbols-outline tree-sitter-langs treemacs-evil treemacs-magit treemacs-projectile treesit-auto ts undo-fu yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
