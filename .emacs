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
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "C-`") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-~") 'copilot-accept-completion-by-word))

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

;; Superstar mode
(use-package org-superstar
  :straight (:host github :repo "integral-dw/org-superstar-mode")
  :ensure t
  :config
  (org-superstar-configure-like-org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq superstar-cycle-sequence '("◉" "◎" "○" "●")))

;; Recentf
(use-package recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 20)


;; Enable spell checking in org-mode
(add-hook 'org-mode-hook 'flyspell-mode)

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

;; Keybindings
(global-set-key (kbd "C-<f1>") 'repeat)

;; -------------------------------------------------
;; 1. Make <f12> a prefix key
;; This define a set of keys of system functionalities related to file and buffer management
;; -------------------------------------------------
(define-prefix-command 'my/f12-map)        ; create the map
(global-set-key (kbd "<f12>") 'my/f12-map) ; bind F12 to it
(define-key my/f12-map (kbd "<f12>") 'save-buffer)   ; F12 F12
(define-key my/f12-map (kbd "<f11>") 'find-file)     ; F12 F11
(define-key my/f12-map (kbd "b") 'switch-to-buffer)  ; F12 b → switch buffer
(define-key my/f12-map (kbd "q") 'kill-this-buffer)      ; F12 q → close frame
(define-key my/f12-map (kbd "g") 'consult-grep)        ; F12 g → grep
(define-key my/f12-map (kbd "k") 'consult-kmacro)	   ; F12 k → consult-macro
(define-key my/f12-map (kbd "r") 'consult-recent-file) ; F12 r → recent files

;; -------------------------------------------------
;; 2. Make <f11> a prefix key
;; This define a set of keys of git functionalities
;; -------------------------------------------------
(define-prefix-command 'my/f11-map)        ; create the map
(global-set-key (kbd "<f11>") 'my/f11-map) ; bind F
(define-key my/f11-map (kbd "<f12>") 'magit-status) ; F11 F12
(define-key my/f11-map (kbd "b") 'magit-blame)
(define-key my/f11-map (kbd "c") 'magit-commit)
(define-key my/f11-map (kbd "p") 'magit-pull)
(define-key my/f11-map (kbd "P") 'magit-push)
(define-key my/f11-map (kbd "s") 'magit-stage-file)
(define-key my/f11-map (kbd "u") 'magit-unstage-file)
(define-key my/f11-map (kbd "l") 'magit-log)
(define-key my/f11-map (kbd "f") 'magit-fetch)
(define-key my/f11-map (kbd "d") 'magit-diff)
(define-key my/f11-map (kbd "o") 'magit-checkout)
(define-key my/f11-map (kbd "r") 'magit-rebase)
(define-key my/f11-map (kbd "a") 'magit-branch)
(define-key my/f11-map (kbd "A") 'magit-stage-modified)

;; -------------------------------------------------
;; 3. Make <f9> a prefix key
;; This define a set of keys of treemacs functionalities
;; -------------------------------------------------
(define-prefix-command 'my/f9-map)        ; create the map
(global-set-key (kbd "<f9>") 'my/f9-map) ; bind F
(define-key my/f10-map (kbd "<f9>") 'treemacs)
(define-key my/f10-map (kbd "w") 'treemacs-create-workspace)
(define-key my/f10-map (kbd "a") 'treemacs-add-project-to-workspace)
(define-key my/f10-map (kbd "s") 'treemacs-switch-workspace)
(define-key my/f10-map (kbd "d") 'treemacs-remove-project-from-workspace)

;; -------------------------------------------------

;; 4. Make <f10> a prefix key
;; This define a set of keys of windows management functionalities
;; -------------------------------------------------
(define-prefix-command 'my/f10-map)        ; create the map
(global-set-key (kbd "<f10>") 'my/f10-map) ; bind F
(define-key my/f10-map (kbd "h") 'split-window-below)
(define-key my/f10-map (kbd "v") 'split-window-right)
(define-key my/f10-map (kbd "<f9>") 'windmove-left)
(define-key my/f10-map (kbd "<f11>") 'windmove-right)
(define-key my/f10-map (kbd "<f10>") 'windmove-down) 
(define-key my/f10-map (kbd "S-<f10>") 'windmove-up)
(define-key my/f10-map (kbd "d") 'delete-other-windows)
(define-key my/f10-map (kbd "h") 'next-window-any-frame)
(define-key my/f10-map (kbd "l") 'previous-window-any-frame)
(define-key my/f10-map (kbd "f") 'toggle-frame-fullscreen)

;; -------------------------------------------------
;; 5. Make <f5> a prefix key
;; This define a set of keys of org-mode functionalities
;; -------------------------------------------------
(define-prefix-command 'my/f5-map)        ; create the map
(global-set-key (kbd "<f5>") 'my/f5-map) ; bind F
(define-key my/f5-map (kbd "<t>") 'org-todo)
(define-key my/f5-map (kbd "<l>") 'org-onsert-link)
