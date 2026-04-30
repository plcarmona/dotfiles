;;; ob-ai.el --- Org-mode AI context system with Ollama/OpenCode backends -*- lexical-binding: t; -*-
(require 'org)
(require 'org-element)
(require 'org-roam)
(require 'url)
(require 'json)
(require 'cl-lib)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defconst ob-ai-config-filename ".ai-config.json"
  "Config file name to search for in directory hierarchy.")

(defconst ob-ai-default-config
  '((model . "granite3.3:latest")
    (backend . "ollama")
    (ollama_host . "localhost:11434")
    (default_context . "self backlinks:1")
    (enhance_context . "self backlinks:1 header!:\"Full Transcript\"")
    (max_context_nodes . 30)
    (max_context_chars . 50000)
    (temperature . 0.3)
    (num_ctx . 8192)
    (opencode_agent . "build")
    (opencode_variant . ""))
  "Built-in defaults used when no config file is found.")

(defvar ob-ai-cached-config nil
  "Cached merged config. Invalidated on dir change.")

(defvar ob-ai-cached-dir nil
  "Directory for which config was cached.")

;;; ============================================================
;;; Config discovery + inheritance (Option C)
;;; ============================================================

(defun ob-ai--walk-up-dirs (dir)
  "Return list of directories from DIR up to ~/."
  (let ((home (expand-file-name "~"))
        result)
    (while (and dir (string-prefix-p home (expand-file-name dir)))
      (push (expand-file-name dir) result)
      (let ((parent (file-name-directory (directory-file-name (expand-file-name dir)))))
        (if (string= (expand-file-name dir) (expand-file-name parent))
            (setq dir nil)
          (setq dir parent))))
    (nreverse result)))

(defun ob-ai-find-configs ()
  "Find all .ai-config.json from current dir up to ~/.
Returns list of paths ordered from ~/ (root) to nearest (highest priority)."
  (let* ((dirs (ob-ai--walk-up-dirs default-directory))
         configs)
    (dolist (dir dirs)
      (let ((path (expand-file-name ob-ai-config-filename dir)))
        (when (file-exists-p path)
          (push path configs))))
    (nreverse configs)))

(defun ob-ai-load-config-raw (path)
  "Load a single config file as alist."
  (condition-case nil
      (let ((data (json-read-file path)))
        (let (result)
          (dolist (key (hash-table-keys data))
            (push (cons (intern key) (gethash key data)) result))
          result))
    (error nil)))

(defun ob-ai-merge-configs (config-list)
  "Merge configs: later entries override earlier ones."
  (let ((merged (copy-alist ob-ai-default-config)))
    (dolist (cfg config-list)
      (dolist (pair cfg)
        (let ((existing (assq (car pair) merged)))
          (if existing
              (setcdr existing (cdr pair))
            (push pair merged)))))
    merged))

(defun ob-ai-load-config ()
  "Load and merge all config files with caching."
  (if (and ob-ai-cached-config
           ob-ai-cached-dir
           (string= ob-ai-cached-dir default-directory))
      ob-ai-cached-config
    (let* ((paths (ob-ai-find-configs))
           (raws (mapcar #'ob-ai-load-config-raw paths))
           (merged (ob-ai-merge-configs raws)))
      (setq ob-ai-cached-config merged
            ob-ai-cached-dir default-directory)
      merged)))

(defun ob-ai-config-get (key)
  "Get config value for KEY (symbol)."
  (cdr (assq key (ob-ai-load-config))))

(defun ob-ai-invalidate-cache ()
  "Invalidate cached config (call after init-config)."
  (setq ob-ai-cached-config nil
        ob-ai-cached-dir nil))

(defun ob-ai-init-config ()
  "Copy resolved merged config to current directory."
  (interactive)
  (let* ((merged (ob-ai-load-config))
         (target (expand-file-name ob-ai-config-filename default-directory))
         (json-encoding-pretty-print t))
    (if (file-exists-p target)
        (message "%s already exists" target)
      (let ((hash (make-hash-table)))
        (dolist (pair merged)
          (puthash (symbol-name (car pair)) (cdr pair) hash))
        (with-temp-file target
          (insert (json-encode hash))))
      (ob-ai-invalidate-cache)
      (message "Created %s" target))))

;;; ============================================================
;;; Fuzzy key resolution
;;; ============================================================

(defconst ob-ai-key-aliases
  '(("ctx"     . context)
    ("mdl"     . model)
    ("mod"     . model)
    ("bck"     . backend)
    ("be"      . backend)
    ("mcc"     . max_context_chars)
    ("mcn"     . max_context_nodes)
    ("tmp"     . temperature)
    ("nctx"    . num_ctx)
    ("agt"     . opencode_agent)
    ("agent"   . opencode_agent)
    ("vrt"     . opencode_variant)
    ("variant" . opencode_variant)
    ("host"    . ollama_host))
  "Alias map: short key string → full config key symbol.")

(defun ob-ai-resolve-key (str)
  "Resolve abbreviated key STR to full config key symbol.
1. Exact match with config key.
2. Alias map match.
3. Unique prefix of a config key."
  (let* ((s (downcase str))
         (sym (intern-soft s)))
    (cond
     ;; Exact config key match
     ((assq sym ob-ai-default-config) sym)
     ;; Alias match
     ((let ((alias (cdr (assoc s ob-ai-key-aliases))))
        (when alias alias))
      (cdr (assoc s ob-ai-key-aliases)))
     ;; Prefix match against config keys
     (t
      (let ((matches
             (cl-remove-if-not
              (lambda (k)
                (string-prefix-p s (symbol-name (car k))))
              ob-ai-default-config)))
        (when (= (length matches) 1)
          (car (car matches))))))))

(defun ob-ai-parse-attrs (attr-string)
  "Parse attribute string from #+BEGIN_user line.
Returns alist of (config-key-symbol . value)."
  (when (or (null attr-string) (string= attr-string ""))
    (setq attr-string ""))
  (let ((attrs nil)
        (regex "\\([a-zA-Z_]+\\)\\s-*=\\s-*\\(?:\"\\([^\"]*\\)\"\\|\\([^ \t\n]+\\)\\)"))
    (with-temp-buffer
      (insert attr-string)
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let* ((raw-key (match-string 1))
               (val (or (match-string 2) (match-string 3)))
               (resolved (ob-ai-resolve-key raw-key)))
          (when resolved
            (if (string-match-p "^[0-9]+$" val)
                (push (cons resolved (string-to-number val)) attrs)
              (push (cons resolved val) attrs))))))
    (nreverse attrs)))

;;; ============================================================
;;; Backend inference
;;; ============================================================

(defun ob-ai--infer-backend (model)
  "Infer backend from MODEL name.
'debug' → debug. 'plan/build/explore/general' → opencode. Else → ollama."
  (let ((m (downcase model)))
    (cond
     ((string= m "debug") 'debug)
     ((member m '("plan" "build" "explore" "general")) 'opencode)
     (t 'ollama))))

(defun ob-ai--normalize-model (model backend)
  "Normalize MODEL for the given BACKEND.
For opencode, MODEL is the agent name. For ollama, MODEL is the model name."
  (if (eq backend 'opencode)
      (or model "build")
    (or model (ob-ai-config-get 'model))))

;;; ============================================================
;;; Org-babel integration (#+begin_src ai)
;;; ============================================================

(defun ob-ai--get-src-block-params ()
  "Get raw key=value params from current src block header."
  (let* ((elem (org-element-at-point))
         (lang (org-element-property :language elem))
         (params (org-element-property :parameters elem)))
    (when (and lang (string= lang "ai"))
      (or params ""))))

(defun ob-ai--get-conversation ()
  "Collect all src ai blocks and results as conversation.
Returns list of (role . text) in buffer order."
  (let (result)
    (org-element-map (org-element-parse-buffer) '(src-block fixed-width)
      (lambda (elem)
        (pcase (org-element-type elem)
          ('src-block
           (when (string= (org-element-property :language elem) "ai")
             (let ((body (string-trim (or (org-element-property :value elem) ""))))
               (when (not (string-empty-p body))
                 (push (cons "user" body) result)))))
          ('fixed-width
           (let ((val (string-trim (or (org-element-property :value elem) ""))))
             (when (not (string-empty-p val))
               (push (cons "assistant" val) result)))))))
    (nreverse result)))

;;; ============================================================
;;; Context gathering via org-roam
;;; ============================================================

(defun ob-ai--get-node-file-content (node)
  "Read full content of NODE's file."
  (let ((file (org-roam-node-file node)))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun ob-ai--filter-headers (content include-regex exclude-regex)
  "Filter CONTENT to include/exclude specific org headings."
  (if (and (not include-regex) (not exclude-regex))
      content
    (with-temp-buffer
      (insert content)
      (org-mode)
      (let ((org-export-preserve-extensions t)
            result keep-current)
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (h)
            (let* ((title (org-element-property :raw-value h))
                   (begin (org-element-property :begin h))
                   (end (org-element-property :end h))
                   (content-begin (org-element-property :contents-begin h))
                   (content-end (org-element-property :contents-end h)))
              (when (and content-begin content-end)
                (let ((section-content
                       (string-trim
                        (buffer-substring-no-properties content-begin content-end)))
                      (include-p (or (null include-regex)
                                     (string-match-p include-regex title)))
                      (exclude-p (and exclude-regex
                                      (string-match-p exclude-regex title))))
                  (when (and include-p (not exclude-p))
                    (push (format "* %s\n%s" title section-content) result)))))))
        (mapconcat #'identity (nreverse result) "\n\n")))))

(defun ob-ai--get-backlinks (node depth &optional visited)
  "Get backlinks of NODE at DEPTH, avoiding VISITED node IDs."
  (let* ((node-id (org-roam-node-id node))
         (visited (or visited (make-hash-table :test 'equal)))
         (backlinks (org-roam-backlinks-get node))
         result)
    (puthash node-id t visited)
    (when (> depth 0)
      (dolist (bl backlinks)
        (let ((src-node (org-roam-backlink-source-node bl))
              (src-id (org-roam-node-id (org-roam-backlink-source-node bl))))
          (unless (gethash src-id visited)
            (let ((content (ob-ai--get-node-file-content src-node)))
              (when content
                (push (format "** Backlink: %s\n%s"
                              (org-roam-node-title src-node)
                              content)
                      result))
               (when (> depth 1)
                 (let ((deeper (ob-ai--get-backlinks src-node (1- depth) visited)))
                   (when deeper (push deeper result)))))))))
    (mapconcat #'identity (nreverse result) "\n\n")))

(defun ob-ai--get-forward-links (node depth &optional visited)
  "Get forward links of NODE at DEPTH."
  (let* ((node-id (org-roam-node-id node))
         (visited (or visited (make-hash-table :test 'equal)))
         (links (org-roam-db-query
                 [:select dest :from links
                  :where (= source $s1)]
                 node-id))
         result)
    (puthash node-id t visited)
    (when (> depth 0)
      (dolist (link links)
        (let* ((dest-id (car link))
               (dest-node (org-roam-node-from-id dest-id)))
          (when (and dest-node
                     (not (gethash dest-id visited)))
            (let ((content (ob-ai--get-node-file-content dest-node)))
              (when content
                (push (format "** Forward: %s\n%s"
                              (org-roam-node-title dest-node)
                              content)
                      result))
              (when (> depth 1)
                (let ((deeper (ob-ai--get-forward-links dest-node (1- depth) visited)))
                  (when deeper (push deeper result)))))))))
    (mapconcat #'identity (nreverse result) "\n\n")))

(defun ob-ai--get-by-tags (tags-str)
  "Get nodes matching any tag in TAGS-STR (comma-separated)."
  (let* ((tags (split-string tags-str "," t "[[:space:]]+"))
         result)
    (dolist (tag tags)
      (let ((nodes (org-roam-db-query
                    [:select nodes:id nodes:title :from nodes
                     :join tags :on (= nodes:id tags:node-id)
                     :where (= tags:tag $s1)]
                    tag)))
        (dolist (n nodes)
          (let ((node (org-roam-node-from-id (car n))))
            (when node
              (let ((content (ob-ai--get-node-file-content node)))
                (when content
                  (push (format "** Tag [%s]: %s\n%s"
                                tag (org-roam-node-title node) content)
                        result))))))))
    (mapconcat #'identity (nreverse result) "\n\n")))

(defun ob-ai-parse-context-spec (spec)
  "Parse context DSL spec string into operations.
Returns list of (op-type . arg) pairs."
  (let ((ops nil)
        (tokens (split-string spec "[[:space:]]+" t)))
    (dolist (token tokens)
      (cond
       ;; self:body
       ((string= token "self:body")
        (push '(self-body) ops))
       ;; self
       ((string= token "self")
        (push '(self) ops))
       ;; header!:"regex"
       ((string-match "^header!:\\(?:\"\\(.+\\)\"\\|\\(.+\\)\\)$" token)
        (push (cons 'header-exclude
                    (or (match-string 1 token) (match-string 2 token))) ops))
       ;; header:"regex"
       ((string-match "^header:\\(?:\"\\(.+\\)\"\\|\\(.+\\)\\)$" token)
        (push (cons 'header-include
                    (or (match-string 1 token) (match-string 2 token))) ops))
       ;; backlinks:N
       ((string-match "^backlinks:\\([0-9]+\\)$" token)
        (push (cons 'backlinks (string-to-number (match-string 1 token))) ops))
       ;; forward:N
       ((string-match "^forward:\\([0-9]+\\)$" token)
        (push (cons 'forward (string-to-number (match-string 1 token))) ops))
       ;; subgraph:N
       ((string-match "^subgraph:\\([0-9]+\\)$" token)
        (push (cons 'subgraph (string-to-number (match-string 1 token))) ops))
       ;; tags:"t1,t2"
       ((string-match "^tags:\\(?:\"\\(.+\\)\"\\|\\(.+\\)\\)$" token)
        (push (cons 'tags (or (match-string 1 token) (match-string 2 token))) ops))))
    (nreverse ops)))

(defun ob-ai-gather-context (spec)
  "Gather context per SPEC string. Returns assembled text."
  (let* ((ops (ob-ai-parse-context-spec spec))
         (node (org-roam-node-at-point))
         (max-chars (ob-ai-config-get 'max_context_chars))
         parts)
    (dolist (op ops)
      (let ((result
             (pcase (car op)
               ('self
                (when node
                  (let ((content (ob-ai--get-node-file-content node)))
                    (format "** Self: %s\n%s" (org-roam-node-title node)
                            (or content "")))))
               ('self-body
                (when node
                  (let ((content (ob-ai--get-node-file-content node)))
                    (ob-ai--filter-headers content nil "Properties\\|Metadata"))))
               ('header-include
                (when node
                  (ob-ai--filter-headers
                   (ob-ai--get-node-file-content node)
                   (cdr op) nil)))
               ('header-exclude
                (when node
                  (ob-ai--filter-headers
                   (ob-ai--get-node-file-content node)
                   nil (cdr op))))
               ('backlinks
                (when node
                  (ob-ai--get-backlinks node (cdr op))))
               ('forward
                (when node
                  (ob-ai--get-forward-links node (cdr op))))
               ('subgraph
                (when node
                  (concat
                   (ob-ai--get-backlinks node (cdr op))
                   "\n\n"
                   (ob-ai--get-forward-links node (cdr op)))))
               ('tags
                (ob-ai--get-by-tags (cdr op))))))
        (when (and result (not (string-empty-p result)))
          (push result parts))))
    (let ((full (mapconcat #'identity (nreverse parts) "\n\n---\n\n")))
      (if (> (length full) max-chars)
          (concat (substring full 0 max-chars) "\n\n[... truncated ...]")
        full))))

;;; ============================================================
;;; Async Ollama backend
;;; ============================================================

(defun ob-ai-send-ollama (prompt callback)
  "Send PROMPT to Ollama asynchronously, call CALLBACK with response."
  (let* ((host (ob-ai-config-get 'ollama_host))
         (model (ob-ai-config-get 'model))
         (temp (ob-ai-config-get 'temperature))
         (num-ctx (ob-ai-config-get 'num_ctx))
         (url (format "http://%s/api/generate" host))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `(("model" . ,model)
             ("prompt" . ,prompt)
             ("stream" . :json-false)
             ("options" . (("temperature" . ,temp)
                           ("num_ctx" . ,num-ctx))))))
         (url-show-status nil))
    (url-retrieve
     (url-generic-parse-url url)
     (lambda (status)
       (unwind-protect
           (save-excursion
             (goto-char (point-min))
             (re-search-forward "\n\n" nil t)
             (let* ((json-str (buffer-substring-no-properties (point) (point-max)))
                    (data (condition-case nil
                              (json-read-from-string json-str)
                            (error nil)))
                    (response (and data (cdr (assq 'response data)))))
               (funcall callback (or response "Error: no response from Ollama"))))
         (kill-buffer))))))

;;; ============================================================
;;; OpenCode CLI backend
;;; ============================================================

(defun ob-ai-send-opencode (prompt callback &optional agent variant)
  "Send PROMPT to OpenCode CLI asynchronously, call CALLBACK with response.
AGENT defaults to config opencode_agent. VARIANT defaults to config."
  (let* ((agent (or agent (ob-ai-config-get 'opencode_agent) "build"))
         (variant (or variant (ob-ai-config-get 'opencode_variant) ""))
         (buf (generate-new-buffer " *ob-ai-opencode*"))
         (args `("run" ,prompt
                 "--agent" ,(if (stringp agent) agent "build")
                 "--format" "json"
                 "--dangerously-skip-permissions"
                 ,@(unless (or (null variant) (string= variant ""))
                     (list "--variant" variant)))))
    (let ((proc
            (make-process
             :name "ob-ai-opencode"
             :buffer buf
             :command (cons "opencode" args)
             :connection-type 'pipe
             :noquery t
             :sentinel
             (lambda (proc event)
               (cond
                ((string-match-p "finished\\|exited" event)
                 (let ((output (with-current-buffer (process-buffer proc)
                                 (buffer-string)))
                       texts)
                   (dolist (line (split-string output "\n" t))
                     (condition-case nil
                         (let ((json-object-type 'alist))
                           (let* ((data (json-read-from-string line))
                                  (type (cdr (assq 'type data)))
                                  (part (cdr (assq 'part data))))
                             (when (and (string= type "text")
                                        (alist-get 'text part))
                               (push (cdr (assq 'text part)) texts))))
                       (error nil)))
                   (funcall callback
                            (if texts
                                (mapconcat #'identity (nreverse texts) "")
                              "Error: no response from OpenCode")))
                 (kill-buffer buf))
                ((string-match-p "\\`failed\\|exited abnormally" event)
                 (funcall callback
                          (format "Error: OpenCode process failed: %s"
                                  (with-current-buffer buf (buffer-string))))
                 (kill-buffer buf)))))))
      ;; Send EOF to stdin — opencode waits for stdin even with args
      (process-send-eof proc))))

;;; ============================================================
;;; Execution (org-babel)
;;; ============================================================

(defun ob-ai--resolve-attrs (attrs)
  "Merge block ATTRS with config defaults.
Returns full alist with all resolved values."
  (let ((cfg (ob-ai-load-config)))
    (dolist (pair attrs)
      (let ((existing (assq (car pair) cfg)))
        (if existing
            (setcdr existing (cdr pair))
          (push pair cfg))))
    cfg))

(defun ob-ai--build-prompt (body context conversation)
  "Assemble full prompt from BODY, CONTEXT, and CONVERSATION history."
  (let ((conv-text
         (when conversation
           (mapconcat
            (lambda (turn)
              (format "[%s]: %s" (upcase (car turn)) (cdr turn)))
            conversation
            "\n\n"))))
    (cond
     ((and context conv-text)
      (format "## Context\n\n%s\n\n## Conversation History\n\n%s\n\n## Current Query\n\n%s"
              context conv-text body))
     (context
      (format "## Context\n\n%s\n\n## Query\n\n%s" context body))
     (conv-text
      (format "## Conversation History\n\n%s\n\n## Current Query\n\n%s"
              conv-text body))
     (t body))))

(defun ob-ai--update-results (response)
  "Update the #+RESULTS: block for the src block at point with RESPONSE."
  (save-excursion
    ;; Go to the beginning of the src block
    (let* ((elem (org-element-at-point))
           (block-end (org-element-property :end elem)))
      (goto-char block-end)
      ;; Check if there's already a RESULTS block
      (if (re-search-forward "^#\\+RESULTS:" (line-end-position 2) t)
          ;; Replace existing results
          (let ((results-start (line-beginning-position))
                (results-end nil))
            (forward-line 1)
            (setq results-end
                  (if (re-search-forward "^#\\+\\|^\\* " nil t)
                      (match-beginning 0)
                    (point-max)))
            (delete-region results-start results-end)
            (insert (format ": %s\n" (string-trim response))))
        ;; Insert new results block
        (insert (format "#+RESULTS:\n: %s\n" (string-trim response)))))))

;;;###autoload
(defun org-babel-execute:ai (body params)
  "Execute AI src block.
BODY is the prompt, PARAMS contains header args.
Async: returns placeholder, updates #+RESULTS: when done."
  (let* ((raw-params (ob-ai--get-src-block-params))
         (attrs (ob-ai-parse-attrs raw-params))
         (resolved (ob-ai--resolve-attrs attrs))
         (model-val (cdr (assq 'model resolved)))
         (backend-inferred (ob-ai--infer-backend (or model-val "")))
         (context-spec (or (cdr (assq 'context resolved))
                           (cdr (assq 'default_context resolved))))
         (context (when context-spec (ob-ai-gather-context context-spec)))
         (conversation (ob-ai--get-conversation))
         (prompt (ob-ai--build-prompt body context conversation))
         (buf (current-buffer))
         (block-marker (set-marker (make-marker)
                        (org-element-property :begin (org-element-at-point)))))
    (if (eq backend-inferred 'debug)
        prompt
      (message "AI query sent (%s, %s)..." backend-inferred model-val)
      ;; Async callback
      (let ((callback
             (lambda (response)
               (condition-case err
                   (with-current-buffer buf
                     (save-excursion
                       (goto-char block-marker)
                       (when (re-search-forward "^#\\+RESULTS:" nil t)
                         (let ((start (line-beginning-position)))
                           (forward-line 1)
                           (let ((end (or (save-excursion
                                            (re-search-forward "^#\\+\\|^\\* " nil t))
                                          (point-max))))
                             (delete-region start end)))
                         (insert (format "#+RESULTS:\n: %s\n" (string-trim response))))
                       (message "AI response received.")))
                 (error (message "AI callback error: %S" err))))))
        (pcase backend-inferred
          ('opencode
           (let ((agent (or model-val "build"))
                 (variant (cdr (assq 'opencode_variant resolved))))
             (ob-ai-send-opencode prompt callback agent variant)))
          ('ollama
           (ob-ai-send-ollama prompt callback))
           (_ (ob-ai-send-ollama prompt callback))))
      ;; Return placeholder immediately (async)
      "processing...")))

(defun ob-ai-enhance-heading ()
  "Enhance the heading at point using AI.
Replaces heading content with AI-generated improvement."
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         (elem (org-element-at-point))
         (content-begin (org-element-property :contents-begin elem))
         (content-end (org-element-property :contents-end elem))
         (content (when (and content-begin content-end)
                    (string-trim
                     (buffer-substring-no-properties content-begin content-end))))
         (context-spec (ob-ai-config-get 'enhance_context))
         (context (when context-spec (ob-ai-gather-context context-spec)))
         (prompt (format
                  "## Context\n\n%s\n\n## Task\n\nImprove the following heading content for \"%s\". Write only the improved content, no explanations.\n\n## Current Content\n\n%s"
                  (or context "")
                  heading
                  (or content "[empty]")))
         (buf (current-buffer))
         (beg content-begin)
         (end content-end)
         (model (ob-ai-config-get 'model))
         (backend (ob-ai--infer-backend model)))
    (message "Enhancing '%s' (%s)..." heading backend)
    (let ((callback
           (lambda (response)
             (with-current-buffer buf
               (save-excursion
                 (goto-char beg)
                 (delete-region beg end)
                 (insert (string-trim response) "\n"))
               (message "Enhanced '%s'" heading)))))
      (pcase backend
        ('opencode (ob-ai-send-opencode prompt callback model))
        (_ (ob-ai-send-ollama prompt callback))))))

;;; ============================================================
;;; Font lock + Setup
;;; ============================================================

(defun ob-ai-setup-font-lock ()
  "Add font-lock keywords for ai src blocks."
  (font-lock-add-keywords
   nil
   '(("#\\+\\(begin\\|end\\)_src ai"
      1 '(:weight bold :foreground "#e45649") t))))

;;;###autoload
(defun ob-ai-setup ()
  "Set up ob-ai mode."
  (ob-ai-setup-font-lock)
  (add-to-list 'org-babel-load-languages '(ai . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (define-key org-mode-map (kbd "C-c a") ob-ai-map))

;;;###autoload
(add-hook 'org-mode-hook #'ob-ai-setup)

(defun ob-ai-quick-query ()
  "Quick query from minibuffer."
  (interactive)
  (let ((query (read-string "AI query: "))
        (context-spec (ob-ai-config-get 'default_context)))
    (let* ((context (when context-spec (ob-ai-gather-context context-spec)))
           (prompt (if context
                       (format "## Context\n\n%s\n\n## Query\n\n%s"
                               context query)
                     query)))
      (message "AI processing...")
      (let ((callback
             (lambda (response)
               (with-current-buffer (get-buffer-create "*ob-ai-result*")
                 (erase-buffer)
                 (insert response)
                 (display-buffer (current-buffer)))
               (message "AI response received."))))
         (pcase (ob-ai--infer-backend (ob-ai-config-get 'model))
           ('opencode (ob-ai-send-opencode prompt callback))
           (_ (ob-ai-send-ollama prompt callback)))))))

;;; ============================================================
;;; Setup
;;; ============================================================

;;;###autoload
(add-hook 'org-mode-hook #'ob-ai-setup)

(defvar ob-ai-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'ob-ai-enhance-heading)
    (define-key map (kbd "i") 'ob-ai-init-config)
    (define-key map (kbd "q") 'ob-ai-quick-query)
    map))

(provide 'ob-ai)
;;; ob-ai.el ends here
