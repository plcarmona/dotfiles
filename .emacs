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

;; Package instalation
;; Copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)

;; Enable src block execution whithout confirmation
(setq org-confirm-babel-evaluate nil)

;; Disable sound
(setq ring-bell-function 'ignore)

;; Magit
(use-package magit)

;; All-the-icons
(use-package all-the-icons)

;; Doom themes
(use-package doom-themes)
(load-theme 'doom-plain-dark t)

;; Treemacs
(use-package treemacs)

;; Consult/Vertico and styling setup
;; Minibuffer completion framework
;; Vertico for vertical completion
;; Consult for enhanced commands
;; Orderless for flexible matching
;; Marginalia for annotations

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package vertico-posframe
  :ensure t
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)
          (border-width . 2))))

(use-package consult
  :ensure t
  :bind (("C-x C-m" . consult-buffer)
         ("C-s" . consult-line)
         ("C-x C-f" . consult-find)))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))
(setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
(setq vertico-posframe-border-width 1)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(defun my/complete-from-list (items &optional prompt initial)
  (interactive)
  (let ((prompt (or prompt "Select item: ")))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook
                    (lambda () (kill-buffer "*Preview*"))
                    nil t))
      (completing-read
       prompt
       items
       nil
       t
       initial
       'my/complete-from-list-history
       nil
       (lambda (cand)
         (with-current-buffer (get-buffer-create "*Preview*")
           (erase-buffer)
           (if (file-exists-p cand)
               (insert-file-contents cand nil 0 1000)
             (insert (format "Preview: %s" cand)))))))))
 
;; Define variables here
;; -------------------------------------------------- ;;
;; Display line numbers
(setq display-line-numbers-type `relative)
(setq display-line-numbers-mode t)
(global-display-line-numbers-mode)
;; Set Font to MonaspaceNeonNF-Regular
(set-face-attribute 'default nil :family "'Monaspace Neon',monospace" :weight 'light :height 150)

;; hide tool-bar and menu-bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Set org directory
(setq default-directory "/Users/sofia/")
