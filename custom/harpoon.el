(setq harpoon-path "~/.emacs.d/dotfiles/custom/harpoon.org")

  (defun save-last-sexp ()
    "copy to kill-ring the last sexp before point."
    (interactive)
    (let ((last-sexp (thing-at-point 'sexp t)))
      (when last-sexp
        (kill-new last-sexp)
        (message "Saved last sexp to kill-ring: %s" last-sexp))))

  (defun harpoon-eval-key (key)
    "test fun to print the submited key"
    (interactive "kPress a key: ")
    (setq key-str (key-description key))
    (message "Pressed key: %s" key-str)
    (run-src-block-named key-str))

  (defun harpoon-key-exist(key)
    "Check if a src block named KEY exists in the harpoon file."
    (interactive "MKey: ")
    (with-temp-buffer
      (insert-file-contents harpoon-path)
      (search-forward (format "#+name: %s" key) nil t)))

  (defun run-src-block-named(key)
    "Run the src block named key in the harpoon file."
    (let ((org-babel-default-header-args:elisp '((:results . "silent"))))
      (with-temp-buffer
        (insert-file-contents harpoon-path)
        (org-mode)
        (org-babel-goto-named-src-block key)
        (org-babel-execute-src-block))))

  (defun delete-src-block-ifp (&optional key)
    "Delete the source block asociated to key"
    (interactive "MKey: ")
    (when key
      (with-temp-buffer
        (insert-file-contents harpoon-path)
        (org-mode)
        (org-babel-goto-named-src-block key)
        ;; find #+name: key to end_src
        (search-backward (format "#+name: %s" key))
        (setq start (point))
        (let ((start (point)))
   	(search-forward "#+end_src")
   	(delete-region start (point)))
        (write-region (point-min) (point-max) harpoon-path))))


  (defun create-src-block-named (key code)
    "Create a src block named KEY with CODE in the harpoon file."
    (with-temp-buffer
      (insert-file-contents harpoon-path)
      (if (harpoon-key-exist key)
   	(delete-src-block-ifp key))        
      (goto-char (point-max))
      (insert (format "\n#+name: %s\n#+begin_src elisp :results none\n%s\n#+end_src\n" key code))
      (write-region (point-min) (point-max) harpoon-path)))

  (defun harpoon-save-lastsexp (key)
    "Create a src block named KEY with the last sexp before point."
    (interactive "MKey: ")
    (let ((last-sexp (thing-at-point 'sexp t)))
      (when last-sexp
        (create-src-block-named key last-sexp)
        (message "Created src block named %s with last sexp." key))))

  (defun harpoon-pick-file (key)
    "Create a src block named KEY that opens the current file."
    (interactive "MKey: ")
    (let ((file-path (buffer-file-name)))
      (when file-path
        (create-src-block-named key (format "(find-file \"%s\")" file-path))
        (message "Created src block named %s to open file %s." key file-path))))

  (defun harpoon-save-last-command (key)
    "Create a src block named KEY with the last executed command."
    (interactive "MKey: ")
    (let ((last-command-str (format "(%s)" last-command)))
      (create-src-block-named key last-command-str)
      (message "Created src block named %s with last command." key)))

  (defun harpoon-list-src ()
    "Prompt to select a named source block from the harpoon file and jump to it."
    (interactive)
    (with-current-buffer (find-file-noselect harpoon-path)
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (let* ((block-names (org-babel-src-block-names))
             (block-name (completing-read "Source block name: " block-names nil t)))
        (org-babel-goto-named-src-block block-name)
        (switch-to-buffer (current-buffer))
        (recenter))))

  (defun harpoon-delete-src ()
    "Prompt to delete a named source block"
    (interactive)
    (with-current-buffer (find-file-noselect harpoon-path)
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (let* ((block-names (org-babel-src-block-names))
  	   (block-name (completing-read "Source block name: " block-names nil t)))
        (delete-src-block-ifp block-name)
        (save-buffer)
        (message "Deleted src block named %s." block-name))))

;;DEFINE prefix key TO HARPOON TO <F11>
(define-prefix-command 'harpoon)
(global-set-key (kbd "C-<f11>") 'harpoon)
(define-key harpoon (kbd "@") 'harpoon-save-lastsexp)
(define-key harpoon (kbd "!") 'harpoon-pick-file)
(define-key harpoon (kbd ".") 'harpoon-save-last-command)
(define-key harpoon (kbd "l") 'harpoon-list-src)
(define-key harpoon (kbd "d") 'harpoon-delete-src)
(global-set-key (kbd "<f11>") 'harpoon-eval-key)
