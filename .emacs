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

;; Treemacs icons
(use-package treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

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
(define-prefix-command 'my/utils-map)        ; create the map
(global-set-key (kbd "<f12>") 'my/utils-map) ; bind F12 to it
(define-key my/utils-map (kbd "<f12>") 'save-buffer)   ; F12 F12
(define-key my/utils-map (kbd "<f11>") 'find-file)     ; F12 F11
(define-key my/utils-map (kbd "b") 'switch-to-buffer)  ; F12 b → switch buffer
(define-key my/utils-map (kbd "q") 'kill-current-buffer)      ; F12 q → close frame
(define-key my/utils-map (kbd "g") 'consult-grep)        ; F12 g → grep
(define-key my/utils-map (kbd "k") 'consult-kmacro)	   ; F12 k → consult-macro
(define-key my/utils-map (kbd "r") 'consult-recent-file) ; F12 r → recent files

;; -------------------------------------------------
;; 2. Make <f11> a prefix key
;; This define a set of keys of git functionalities
;; -------------------------------------------------
(define-prefix-command 'my/f11-map)
(global-set-key (kbd "<f11>") 'my/git-map) 
(define-key my/git-map (kbd "<f12>") 'magit-status) 
(define-key my/git-map (kbd "b") 'magit-blame)
(define-key my/git-map (kbd "c") 'magit-commit)
(define-key my/git-map (kbd "p") 'magit-pull)
(define-key my/git-map (kbd "P") 'magit-push)
(define-key my/git-map (kbd "s") 'magit-stage-file)
(define-key my/git-map (kbd "u") 'magit-unstage-file)
(define-key my/git-map (kbd "l") 'magit-log)
(define-key my/git-map (kbd "f") 'magit-fetch)
(define-key my/git-map (kbd "d") 'magit-diff)
(define-key my/git-map (kbd "o") 'magit-checkout)
(define-key my/git-map (kbd "r") 'magit-rebase)
(define-key my/git-map (kbd "a") 'magit-branch)
(define-key my/git-map (kbd "A") 'magit-stage-modified)

;; -------------------------------------------------
;; 3. Make <f9> a prefix key
;; This define a set of keys of treemacs functionalities
;; -------------------------------------------------
(define-prefix-command 'my/tremacs-map)        ; create the map
(global-set-key (kbd "<f9>") 'my/tremacs-map) ; bind F
(define-key my/tremacs-map (kbd "<f9>") 'treemacs)
(define-key my/tremacs-map (kbd "w") 'treemacs-create-workspace)
(define-key my/tremacs-map (kbd "a") 'treemacs-add-project-to-workspace)
(define-key my/tremacs-map (kbd "s") 'treemacs-switch-workspace)
(define-key my/tremacs-map (kbd "d") 'treemacs-remove-project-from-workspace)
(define-key my/tremacs-map (kbd "c") '(lambda () (interactive) (find-file "~/.emacs")))
;; -------------------------------------------------

;; 4. Make <f10> a prefix key
;; This define a set of keys of windows management functionalities
;; esta esquisito 
;; -------------------------------------------------
(define-prefix-command 'my/win-map)        
(global-set-key (kbd "<f8>") 'my/win-map) 
(define-key my/win-map (kbd "z") 'split-window-below)
(define-key my/win-map (kbd "v") 'split-window-right)
(define-key my/win-map (kbd "h") 'windmove-left)
(define-key my/win-map (kbd "l") 'windmove-right)
(define-key my/win-map (kbd "k") 'windmove-down) 
(define-key my/win-map (kbd "j") 'windmove-up)
(define-key my/win-map (kbd "d") 'delete-other-windows)
(define-key my/win-map (kbd "n") 'next-window-any-frame)
(define-key my/win-map (kbd "p") 'previous-window-any-frame)
(define-key my/win-map (kbd "f") 'toggle-frame-fullscreen)
(define-key my/win-map (kbd "<f9>") 'previous-buffer)
(define-key my/win-map (kbd "<win>") 'other-window)
(define-key my/win-map (kbd "<f11>") 'next-buffer)

;; -------------------------------------------------
;; 5. Make <f5> a prefix key
;; This define a set of keys of miscellaneous things
;; -------------------------------------------------
(define-prefix-command 'my/misc-map)
(global-set-key (kbd "<f5>") 'my/misc-map) 
(define-key my/misc-map (kbd "t") 'org-todo)
(define-key my/misc-map (kbd "l") 'org-onsert-link)
(define-key my/misc-map (kbd "c") 'copilot-mode)
(define-key my/misc-map (kbd "s") 'async-shell-command)
