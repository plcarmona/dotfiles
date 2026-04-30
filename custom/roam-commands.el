;;; roam-commands.el --- org-roam heading pickers + semantic search -*- lexical-binding: t; -*-

;; Three commands for jumping around the org-roam knowledge base:
;;
;;   C-c n h  my/roam-select-heading          pick ANY heading across all notes
;;   C-c n H  my/roam-select-heading-by-tag   pick a tag, then a heading
;;   C-c n s  my/roam-semantic-search         dynamic meaning-search (needs server)
;;
;; Tags work on BOTH notes (file-level, from org-roam.db) and HEADERS
;; (parsed from the headline line `* Heading :tag:'). Tag inheritance is ON,
;; so a tag on a note also matches every heading under it (org's default rule).

;;; Code:

(require 'org-roam)
(require 'org)
(require 'consult)
(require 'url)
(require 'json)
(require 'cl-lib)


;;; --- Customization ----------------------------------------------------------

(defgroup my-roam nil
  "Custom org-roam navigation commands."
  :group 'org-roam)

(defcustom my/roam-search-url "http://127.0.0.1:8765"
  "Base URL of the mcp-roam semantic search HTTP endpoint."
  :type 'string
  :group 'my-roam)

(defcustom my/roam-search-start-command
  '("uv" "run" "--directory" "/home/pit/utils/mcp-roam"
    "python" "-m" "mcp_roam.search_http")
  "argv list that starts the semantic search server."
  :type '(repeat string)
  :group 'my-roam)

(defcustom my/roam-search-min-chars 3
  "Minimum input length before a semantic query is fired."
  :type 'integer
  :group 'my-roam)

(defcustom my/roam-search-result-count 12
  "How many results the semantic search returns."
  :type 'integer
  :group 'my-roam)


;;; --- DB helpers -------------------------------------------------------------

(defun my/roam--unquote (s)
  "Strip the surrounding double quotes org-roam stores in the DB."
  (if (and (stringp s) (>= (length s) 2)
           (= (aref s 0) ?\") (= (aref s (1- (length s))) ?\"))
      (substring s 1 -1)
    s))

(defun my/roam--real-org-files ()
  "List of real .org files tracked by org-roam (paths unquoted)."
  (cl-remove-if-not
   (lambda (file)
     (let ((f (my/roam--unquote file)))
       (and (string-suffix-p ".org" f)
            (not (string-prefix-p ".#" (file-name-nondirectory f)))
            (file-exists-p f))))
   (mapcar (lambda (row) (elt row 0))
           (org-roam-db-query [:select file :from files]))))

(defun my/roam--file-titles ()
  "Return a hash table: unquoted file path -> title."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (row (org-roam-db-query [:select [file title] :from files]))
      (let ((file (my/roam--unquote (elt row 0)))
            (title (my/roam--unquote (elt row 1))))
        (puthash file (or title (file-name-base file)) h)))
    h))


;;; --- Heading parsing (cached by file mtime) ---------------------------------

(defvar my/roam--heading-cache (make-hash-table :test 'equal)
  "Cache: \"file\\0mtime\" -> list of (LEVEL TEXT LINE TAGS).")

(defun my/roam--split-heading-tags (body)
  "Return (TEXT . TAGS) from a headline BODY (text after the stars)."
  (let ((text body) (tags nil))
    (save-match-data
      ;; trailing tag group:  :tag1:tag2:   possibly preceded by whitespace
      (when (string-match "[ \t]+\\(:[[:alnum:]_@]+:\\)+[ \t]*\\'" body)
        (let ((raw (match-string 0 body)))
          (setq tags (split-string raw "[:]" t " \t"))
          (setq text (substring body 0 (match-beginning 0)))))
      (cons (string-trim text) tags))))

(defun my/roam--parse-headings (file)
  "Return cached headings of FILE: list of (LEVEL TEXT LINE TAGS)."
  (let* ((attr (file-attributes file))
         (mtime (float-time (file-attribute-modification-time attr)))
         (key (concat file "\0" (number-to-string mtime))))
    (or (gethash key my/roam--heading-cache)
        (let (out)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (let* ((level (length (match-string 1)))
                     (line (line-number-at-pos (line-beginning-position)))
                     (body (buffer-substring-no-properties
                            (match-end 0) (line-end-position)))
                     (split (my/roam--split-heading-tags body)))
                (push (list level (car split) line (cdr split)) out))))
          (let ((headings (nreverse out)))
            (puthash key headings my/roam--heading-cache)
            headings)))))

(defun my/roam--goto-heading (cand)
  "Open the file recorded in CAND and jump to its line."
  (when cand
    (pcase-let ((`(,file . ,line) (get-text-property 0 'my-jump cand)))
      (when (and file (file-exists-p file))
        (find-file file)
        (widen)
        (goto-char (point-min))
        (forward-line (1- line))
        (recenter 0)))))


;;; --- Heading candidates -----------------------------------------------------

(defun my/roam--heading-candidates (&optional file-filter titles)
  "Build consult candidates from headings.
FILE-FILTER, if a hash table, restricts to those file paths.
TITLES is a hash file->title (computed once, reused)."
  (let* ((titles (or titles (my/roam--file-titles)))
         cands)
    (dolist (file (my/roam--real-org-files))
      (when (or (null file-filter) (gethash file file-filter))
        (let ((title (gethash file titles (file-name-base file))))
          (dolist (h (my/roam--parse-headings file))
            (let* ((level (nth 0 h))
                   (text  (nth 1 h))
                   (line  (nth 2 h))
                   (tags  (nth 3 h))
                   (indent (make-string (max 0 (1- level)) ?\s))
                   (tagstr (if tags
                               (propertize
                                (concat "  :" (mapconcat #'identity tags ":") ":")
                                'face 'shadow)
                             ""))
                   (cand (concat indent title "  ❯  " text tagstr)))
              (put-text-property 0 (length cand) 'my-jump (cons file line) cand)
              (put-text-property 0 (length cand) 'my-tags tags cand)
              (push cand cands))))))
    (nreverse cands)))


;;; --- Tag index (note tags + header tags, with inheritance) ------------------

(defun my/roam--tag-index ()
  "Return (TAG-COUNTS . TAG->MATCHES).
TAG-COUNTS: alist (tag . count), most-used first.
TAG->MATCHES: hash tag -> list of (file . line) headings matching that tag
              (a heading matches its OWN tags OR its file's tags — inheritance)."
  (let* ((file->tags (make-hash-table :test 'equal))   ; file -> (tag ...)
         (tag->matches (make-hash-table :test 'equal)) ; tag -> ((file . line) ...)
         (counts (make-hash-table :test 'equal))
         (id->file (make-hash-table :test 'equal)))
    ;; 1. file-level tags from the org-roam DB
    (dolist (row (org-roam-db-query [:select [id file] :from nodes]))
      (let ((file (my/roam--unquote (elt row 1))))
        (when (and file (string-suffix-p ".org" file)
                   (not (string-prefix-p ".#" (file-name-nondirectory file))))
          (puthash (my/roam--unquote (elt row 0)) file id->file))))
    (dolist (row (org-roam-db-query [:select [node_id tag] :from tags]))
      (let* ((tag (my/roam--unquote (elt row 1)))
             (file (gethash (my/roam--unquote (elt row 0)) id->file)))
        (when (and tag file)
          (cl-pushnew tag (gethash file file->tags nil) :test #'equal))))
    ;; 2. walk every note: inherit file tags + collect header tags
    (dolist (file (hash-table-values id->file))
      (let ((ftags (gethash file file->tags)))
        (dolist (h (my/roam--parse-headings file))
          (let* ((line (nth 2 h))
                 (htags (nth 3 h))
                 (all (cl-union htags ftags :test #'equal)))
            (dolist (tag all)
              (cl-pushnew (cons file line)
                          (gethash tag tag->matches nil) :test #'equal)
              (puthash tag (1+ (gethash tag counts 0)) counts))))))
    (let (counts-alist)
      (maphash (lambda (k v) (push (cons k v) counts-alist)) counts)
      (cons (sort counts-alist (lambda (a b) (> (cdr a) (cdr b))))
            tag->matches))))


;;; --- Public commands --------------------------------------------------------

;;;###autoload
(defun my/roam-select-heading ()
  "Select any heading across all org-roam notes and jump to it."
  (interactive)
  (message "Collecting headings...")
  (let ((cands (my/roam--heading-candidates)))
    (if (null cands)
        (message "No headings found in org-roam notes.")
      (message nil)
      (my/roam--goto-heading
       (consult--read cands
         :prompt "Heading: "
         :sort nil
         :require-match t
         :category 'my-roam-heading)))))

;;;###autoload
(defun my/roam-select-heading-by-tag ()
  "Pick a tag, then pick a heading among notes/headers carrying that tag.
Tags on notes are inherited by their headers; tags on headers match directly."
  (interactive)
  (message "Indexing tags...")
  (pcase-let ((`(,counts . ,tag->matches) (my/roam--tag-index)))
    (if (null counts)
        (message "No tags found (add :tag: to notes or headers).")
      (message nil)
      (let* ((tag-cands (mapcar (lambda (c) (format "%-24s %d" (car c) (cdr c))) counts))
             (chosen (consult--read tag-cands
                       :prompt "Tag: "
                       :sort nil
                       :require-match t)))
        (when chosen
          (let* ((tag (car (split-string (string-trim chosen) "[ \t]" t)))
                 (matches (gethash tag tag->matches))
                 ;; restrict the heading list to matching (file . line) pairs
                 (match-set (let ((h (make-hash-table :test 'equal)))
                              (dolist (m matches) (puthash (car m) t h))
                              h))
                 (cands (my/roam--heading-candidates match-set)))
            (if (null cands)
                (message "No headings under tag %s" tag)
              (my/roam--goto-heading
                 (consult--read cands
                  :prompt (format "Heading [%s]: " tag)
                  :sort nil
                  :require-match t
                  :category 'my-roam-heading)))))))))


;;; --- Semantic search (dynamic, via localhost HTTP server) -------------------

(defun my/roam--server-alive-p ()
  "Return non-nil if the search server answers /health."
  (condition-case nil
      (let ((url-request-method "GET")
            (url-show-status nil)
            (buf (url-retrieve-synchronously
                  (concat my/roam-search-url "/health") t nil 1.5)))
        (prog1 (and buf (buffer-live-p buf))
          (when buf (kill-buffer buf))))
    (error nil)))

(defun my/roam--ensure-server ()
  "Start the semantic search server if it isn't running. Return non-nil on success."
  (or (my/roam--server-alive-p)
      (let ((proc (apply #'start-process "roam-search" "*roam-search-log*"
                         my/roam-search-start-command)))
        (set-process-query-on-exit-flag proc nil))
      (let ((tries 40))
        (while (and (not (my/roam--server-alive-p)) (> tries 0))
          (sit-for 0.1)
          (cl-decf tries)))
      (my/roam--server-alive-p)))

(defun my/roam--semantic-fetch (query &optional k rerank)
  "POST QUERY to the server; return a list of plists, or nil."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode `((query . ,query)
                         (k . ,(or k 12))
                         (rerank . ,(if rerank t :json-false)))))
         (url-show-status nil)
         (buf (url-retrieve-synchronously
               (concat my/roam-search-url "/search") t nil 6)))
    (if (not buf)
        (progn (message "roam semantic server unreachable") nil)
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (if (not (re-search-forward "\r?\n\r?\n" nil t))
                (progn (message "bad HTTP response from search server") nil)
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (data (json-read))
                     (results (cdr (assq 'results data))))
                (mapcar
                 (lambda (r)
                   `(:node_id ,(cdr (assq 'node_id r))
                     :title ,(cdr (assq 'title r))
                     :file ,(cdr (assq 'file r))
                     :heading_path ,(cdr (assq 'heading_path r))
                     :text ,(cdr (assq 'text r))
                     :distance ,(cdr (assq 'distance r))
                     :rerank_score ,(cdr (assq 'rerank_score r))))
                 results))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defvar my/roam--semantic-cache (make-hash-table :test 'equal)
  "Cand-string -> (file . heading_path), refreshed per search.")

(defun my/roam--semantic-format (r)
  "Turn a result plist R into a displayable candidate; cache its jump target."
  (let* ((title (or (plist-get r :title) "?"))
         (path  (or (plist-get r :heading_path) ""))
         (text  (or (plist-get r :text) ""))
         (file  (plist-get r :file))
         (text1 (replace-regexp-in-string "[\r\n]+" " ⏎ " text))
         (cand (format "%s ❯ %s"
                       (truncate-string-to-width title 30 nil nil t)
                       (truncate-string-to-width text1 70 nil nil t))))
    (puthash cand (cons file path) my/roam--semantic-cache)
    cand))

(defun my/roam--goto-semantic (cand)
  "Open the note for CAND; jump to the matched heading if possible."
  (when cand
    (pcase-let ((`(,file . ,path) (gethash cand my/roam--semantic-cache)))
      (when (and file (file-exists-p file))
        (find-file file)
        (widen)
        (goto-char (point-min))
        (when (and path (derived-mode-p 'org-mode)
                   (string-match "\\([^>]+\\)\\'" path))
          (let ((head (string-trim (match-string 1 path))))
            (ignore-errors (org-link-search head))))))))

;;;###autoload
(defun my/roam-semantic-search ()
  "Dynamic semantic search across org-roam notes (needs the search server).
Starts the server automatically if it isn't running. Results update as you
type (after the first few characters); the listing is also narrowed by your
input, so add/remove words to refine."
  (interactive)
  (unless (my/roam--ensure-server)
    (user-error "Couldn't start the roam search server (see *roam-search-log*)"))
  (clrhash my/roam--semantic-cache)
  (let ((sel (consult--read
              (consult--dynamic-collection
                (lambda (input)
                  (if (< (length input) my/roam-search-min-chars)
                      nil
                    (mapcar #'my/roam--semantic-format
                            (my/roam--semantic-fetch
                             input my/roam-search-result-count nil))))
                :min-input my/roam-search-min-chars
                :debounce 0.25
                :throttle 0.4)
              :prompt "Semantic: "
              :sort nil
              :category 'my-roam-semantic)))
    (when sel (my/roam--goto-semantic sel))))


;;; --- Key bindings -----------------------------------------------------------

(with-eval-after-load 'org-roam
  (define-key org-roam-mode-map (kbd "C-c n h") #'my/roam-select-heading)
  (define-key org-roam-mode-map (kbd "C-c n H") #'my/roam-select-heading-by-tag)
  (define-key org-roam-mode-map (kbd "C-c n s") #'my/roam-semantic-search))

(global-set-key (kbd "C-c n h") #'my/roam-select-heading)
(global-set-key (kbd "C-c n H") #'my/roam-select-heading-by-tag)
(global-set-key (kbd "C-c n s") #'my/roam-semantic-search)

(provide 'roam-commands)
;;; roam-commands.el ends here
