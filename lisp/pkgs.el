(setq user-packages (ht))

(defun make-user-package (name &rest args)
  (set% user-packages name (last@ (apply 'parse-arguments args))))

(defun user-package->use-package (name form)
  (flatten-arguments form (list name)))

(defun eval-user-package (name form)
  (eval (user-package->use-package name form)))

(defmacro package! (name &rest args)
  (apply 'make-user-package name args))

(defun eval-user-packages ()
  (each% user-packages
	 (lambda (name form)
	   (eval (user-package->use-package name form)))))

(defun load-user-packages-file ()
  (when (file-exists-p "~/.emacs.d/pkgs.el")
    (load-file "~/.emacs.d/pkgs.el")))
