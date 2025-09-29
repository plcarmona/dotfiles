   (setq package-enable-at-startup nil)
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b21e3ec892646747d647b34162e9d9e72abfd02ba60dd6e8fc51b2cc65d379dd"
     "7a3ba1a9dd6486f8da0bd486fe7069997c8d5cbc81297106db8d3f5ecf16a60c"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(action-lock-face ((t :inherit button)))
 '(fringe ((t (:background "#282828"))))
 '(howm-mode-keyword-face ((t :inherit org-link)))
 '(howm-mode-ref-face ((t :inherit org-link)))
 '(howm-mode-title-face ((t :inherit org-level-1)))
 '(howm-mode-wiki-face ((t :inherit org-link)))
 '(howm-reminder-deadline-face ((t :inherit org-scheduled-today)))
 '(howm-reminder-defer-face ((t :inherit org-scheduled)))
 '(howm-reminder-done-face ((t :inherit org-done)))
 '(howm-reminder-late-deadline-face ((t :inherit bold :inherit org-imminent-deadline)))
 '(howm-reminder-normal-face ((t :inherit org-default)))
 '(howm-reminder-scheduled-face ((t :inherit org-scheduled)))
 '(howm-reminder-today-face ((t :inherit bold :inherit org-scheduled-today)))
 '(howm-reminder-todo-face ((t :inherit org-todo)))
 '(howm-reminder-tomorrow-face ((t :inherit bold :inherit org-scheduled)))
 '(howm-simulate-todo-mode-line-face ((t :inherit bold)))
 '(howm-view-empty-face ((t :inherit shadow)))
 '(howm-view-hilit-face ((t :inherit isearch)))
 '(howm-view-name-face ((t :inherit org-document-title))))
(org-babel-load-file (expand-file-name "~/cfg.org"))
