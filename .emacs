;; Straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))     
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)
(straight-use-package 'use-package)
;; load org
(straight-use-package 'org)
(straight-use-package 'hyperbole)
;;hyperbole setup
(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save/" t)))
(hyperbole-mode 1)
(global-visual-line-mode 1)

;; -------------------------------------------------
;; -------------------------------------------------
(org-babel-load-file
 (expand-file-name "dotfiles/packages.org"
		   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "dotfiles/keybindings.org"
		   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "dotfiles/custom.org"
		   user-emacs-directory))

;; -------------------------------------------------

;; Load all .el files in ./custom
(let ((custom-dir (expand-file-name "dotfiles/custom" user-emacs-directory)))
  (when (file-exists-p custom-dir)
	(dolist (file (directory-files custom-dir t "\\.el$"))
	  (load file))))
