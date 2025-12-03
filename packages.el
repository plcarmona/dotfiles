;; Package instalation
;; Copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "C-`") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-~") 'copilot-accept-completion-by-word))
;; Disable indent warning
(setq copilot-disable-prompt-on-indent t)
(setq copilot-indent-offset-warning-disable 1)
;; Magit
(use-package magit)

;; All-the-icons
(use-package all-the-icons)

;; Doom themes
(use-package doom-themes)
(load-theme 'doom-one t)

;; ;; Treemacs
(use-package treemacs)

;; ;; Treemacs icons
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

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . prettify-symbols-mode)
  :custom
  (prettify-symbols-alist
   '(("#" . "•")    ; Level 1 heading
     ("##" . "▪")   ; Level 2
     ("###" . "▫")  ; Level 3
     ("####" . "⁃") ; Level 4
     ("*" . "•")    ; Bullet list
     ("+" . "◦")    ; Alternative
     ("-" . "–")
     ("=>". "⇒")
     ("<=" . "⇐")
     ("lambda" . "λ")
     ("->" . "→"))))

;; Enable src block execution in markdown
(use-package md-babel
  :straight (:host github :repo "md-babel/md-babel.el")
  :ensure t
  :after markdown-mode
  :config
  (define-key markdown-mode-command-map (kbd "C-c") #'md-babel-execute-block-at-point)) 

;; Recentf
(use-package recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 20)


;; Enable spell checking in org-mode
(add-hook 'org-mode-hook 'flyspell-mode)

(use-package centered-window
  :config
  (centered-window-mode t))

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
   (set-face-attribute 'default nil :height 180 :weight 'normal :family "MonaspaceNeonNF-Regular")
   ;; hide tool-bar and menu-bar
   (tool-bar-mode -1)
   (menu-bar-mode -1)

;; scratch as initial buffer
(setq initial-buffer-choice t)

;; Set def directory
(setq default-directory "~/")

;; Enable shift selection in org-mode
(setq org-support-shift-select t)

;; Set python interpreter for org-babel
(setq org-babel-python-command "uv run python")

;; Enable src block execution whithout confirmation
 (setq org-confirm-babel-evaluate nil)

 ;; Disable sound
 (setq ring-bell-function 'ignore)

;; ------------------------------------------------------------
;; 1. LSP Mode for Intelligence (pyright)
;; ------------------------------------------------------------
(use-package lsp-mode
  :commands lsp lsp-deferred
  :hook (python-mode . lsp-deferred) ; Start pyright automatically for Python
  :config
  (setq lsp-diagnostics-provider :flymake)
  ;; Optional: Reduce noise
  (setq lsp-auto-guess-root nil)
  (setq lsp-log-io nil)
  )

;; Use pyright as the LSP server for Python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          ;; Optional: Disable pyright's formatting if you want ruff to handle it
                          (setq-local lsp-pyright-disable-diagnostics t) ; Let ruff handle diagnostics
                          (setq-local lsp-pyright-disable-organize-imports t) ; Let ruff handle imports
                          ))
  :config
  ;; Use ruff for diagnostics instead of pyright
  (lsp-register-custom-settings
   '(("pyright.disableDiagnostics" t t)))
  )

;; ------------------------------------------------------------
;; 2. Company for Autocompletion (provided by pyright)
;; ------------------------------------------------------------
(use-package company
  :ensure t
  :hook (lsp-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  )

(use-package pyvenv)

;; Emacs As Terminal (EAT) package installation
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))
