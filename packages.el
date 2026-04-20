;; Package instalation
 ;; Copilot
 (use-package copilot
   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
   :ensure t
   :hook (prog-mode . copilot-mode)
   :config
   (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion)
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
 (setq recentf-max-saved-items 50)

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

;; (use-package vertico-posframe
;;   :ensure t
;;   :config
;;   (vertico-posframe-mode 1)
;;   (setq vertico-posframe-parameters
;;         '((left-fringe . 8)
;;           (right-fringe . 8)
;;           (border-width . 2))))

(use-package consult
  :ensure t
  :bind (("C-x C-m" . consult-buffer)
         ("C-s" . consult-line)
         ("C-x C-f" . consult-find)))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))
;; (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
;; (setq vertico-posframe-border-width 1)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode 1))   ; el 1 es más explícito

;; === REMAPEO CORRECTO PARA PYTHON (importante) ===
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; === Configuración limpia del font-lock para Python ===
(add-hook 'python-ts-mode-hook
          (lambda ()
            ;; Bajamos el nivel de highlighting para evitar el bug del query "self"
            (setq-local treesit-font-lock-level 2)

            ;; Opcional: si querés un control más fino (recomendado)
            ;; (setq-local treesit-font-lock-feature-list
            ;;             '((comment definition)
            ;;               (keyword string number type)
            ;;               (assignment builtin constant)
            ;;               (function bracket delimiter operator variable)))
            ))

;; Si usás lsp-mode / eglot, usá el hook base (funciona para ambos modos)
(add-hook 'python-base-mode-hook 'lsp-deferred)     ; si usás lsp-mode
;; o
;; (add-hook 'python-base-mode-hook 'eglot-ensure)   ; si usás eglot
;;(treesit-install-language-grammar 'python)

;; ------------------------------------------------------------
  ;; 1. LSP Mode for Intelligence (pyright)
  ;; ------------------------------------------------------------
  (use-package lsp-mode
    :commands lsp lsp-deferred
    :hook (python-ts-mode . lsp-deferred) ; Start pyright automatically for Python
    :config
    (setq lsp-diagnostics-provider :flymake)
    (setq lsp-auto-guess-root nil)
    (setq lsp-log-io nil)
    (setq lsp-enable-snippet nil))


  ;; Use pyright as the LSP server for Python
  (use-package lsp-pyright
    :ensure t
    :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            ;; Optional: Disable pyright's formatting if you want ruff to handle it
                            (setq-local lsp-pyright-disable-diagnostics t) ; Let ruff handle diagnostics
                            (setq-local lsp-pyright-disable-organize-imports t))) ; Let ruff handle imports
  			  
    :config
    ;; Use ruff for diagnostics instead of pyright
    (lsp-register-custom-settings
     '(("pyright.disableDiagnostics" t t)))
    )

  ;; ------------------------------------------------------------
  ;; 2. Company for Autocompletion (provided by pyright)
  ;; ------------------------------------------------------------
  ;; (use-package company
  ;;   :ensure t
  ;;   :hook (lsp-mode . company-mode)
  ;;   :config
  ;;   (setq company-minimum-prefix-length 1)
  ;;   (setq company-tooltip-align-annotations t)
  ;;   )
;; O más agresivo (desactiva todo lo custom de python-ts):

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
(setq font-variable "PT Mono")
;; if font variable exist, set to that font else set to MonaspaceNeonNF-Regular
(if (member font-variable (font-family-list))
    (set-face-attribute 'default nil :height 100 :weight 'normal :family font-variable)
  (set-face-attribute 'default nil :height 100 :weight 'normal :family "MonospaceNeonNF-Regular"))


  
   ;; hide tool-bar and menu-bar
   (tool-bar-mode -1)
   (menu-bar-mode -1)

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

(use-package ox-reveal)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@4.3.1/")
(setq org-reveal-theme "black")

(use-package pyvenv)
(pyvenv-tracking-mode 1)
