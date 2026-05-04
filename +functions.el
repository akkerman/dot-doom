;;; +functions.el --- Custom functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom functions for viewsource.nl Doom Emacs configuration.
;; All functions use the `viewsource/' namespace prefix.

;;; Code:

;; Silence byte-compiler warnings for external functions
(declare-function citar-select-ref "citar")
(declare-function citar--format-entry-no-widths "citar")
(declare-function url-retrieve-synchronously "url")
(declare-function libxml-parse-html-region "xml")
(declare-function org-roam-capture- "org-roam-capture")
(declare-function org-roam-node-create "org-roam-node")
(declare-function org-roam-node-title "org-roam-node")
(declare-function org-roam-node-aliases "org-roam-node")
(declare-function org-roam-node-file "org-roam-node")
(declare-function org-roam-unlinked-references--rg-command "org-roam-mode")
(declare-function org-roam-unlinked-references-preview-line "org-roam-mode")
(declare-function org-roam-fontify-like-in-org-mode "org-roam-utils")
(declare-function magit-insert-section "magit-section")
(declare-function magit-insert-heading "magit-section")
(declare-function oset "eieio")
(declare-function org-display-inline-images "org")

;;;; Org-roam & Citations

(defun viewsource/org-roam-node-from-cite (keys-entries)
  "Create an org-roam node from a bibliographic citation.
KEYS-ENTRIES is a cons cell of (citekey . entry) from citar."
  (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
  (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                              "${author editor}. (${date year}). ${title}")))
    (org-roam-capture- :templates
                       '(("r" "reference" plain (file "~/.config/doom/templates/reference.org") :if-new
                          (file+head "refs/${citekey}.org"
                                     ":PROPERTIES:\n:ROAM_REFS: @${citekey}\n:END:\n#+title: ${title}\n")
                          :immediate-finish t
                          :unnarrowed t))
                       :info (list :citekey (car keys-entries))
                       :node (org-roam-node-create :title title)
                       :props '(:finalize find-file))))

;;;; Org-roam & Web URLs

(defun viewsource/--html-node-text (node)
  "Recursively extract all text content from a parsed HTML NODE."
  (cond
   ((stringp node) node)
   ((listp node) (mapconcat #'viewsource/--html-node-text (cddr node) " "))
   (t "")))

(defun viewsource/--html-find-tag (tag tree)
  "Find the first occurrence of TAG in the HTML parse TREE (depth-first)."
  (when (listp tree)
    (if (eq (car tree) tag)
        tree
      (cl-some (lambda (child) (viewsource/--html-find-tag tag child))
               (cddr tree)))))

(defun viewsource/--html-find-all-tags (tags tree)
  "Find all occurrences of any tag in TAGS within the HTML parse TREE.
Returns a list of (tag . text) pairs."
  (when (listp tree)
    (let ((results '()))
      (when (memq (car tree) tags)
        (push (cons (car tree) (string-trim (viewsource/--html-node-text tree)))
              results))
      (dolist (child (cddr tree))
        (setq results (append results (viewsource/--html-find-all-tags tags child))))
      results)))

(defun viewsource/--url-fetch-and-parse (url)
  "Fetch URL and return a cons of (title . headings).
HEADINGS is a list of (tag . text) pairs for h1/h2."
  (let* ((buf (url-retrieve-synchronously url t t 10))
         (tree (when buf
                 (with-current-buffer buf
                   (goto-char (point-min))
                   (when (boundp 'url-http-end-of-headers)
                     (goto-char url-http-end-of-headers))
                   (libxml-parse-html-region (point) (point-max))))))
    (when buf (kill-buffer buf))
    (when tree
      (let* ((title-node (viewsource/--html-find-tag 'title tree))
             (title (if title-node
                        (string-trim (viewsource/--html-node-text title-node))
                      url))
             (headings (viewsource/--html-find-all-tags '(h1 h2) tree)))
        (cons title headings)))))

(defun viewsource/org-roam-node-from-url (url)
  "Create an org-roam node from a web URL.
Fetches the page, extracts title and h1/h2 headings, and creates a ref
note in refs/ with ROAM_REFS set to URL and filetag :lit:."
  (interactive
   (list (let ((clip (ignore-errors (current-kill 0 t))))
           (read-string "URL: "
                        (when (and clip (string-match-p "^https?://" clip)) clip)))))
  (message "Fetching %s..." url)
  (let* ((parsed (viewsource/--url-fetch-and-parse url))
         (_ (unless parsed (user-error "Failed to fetch %s" url)))
         (title (car parsed))
         (headings (cdr parsed))
         (skeleton (if headings
                       (mapconcat (lambda (h)
                                    (format "%s TODO %s\n\n"
                                            (if (eq (car h) 'h1) "*" "**")
                                            (cdr h)))
                                  headings "")
                     "* TODO Notes\n\n%?\n"))
         (slug (org-roam-node-slug (org-roam-node-create :title title)))
         (file (expand-file-name
                (format "%s-%s.org" (format-time-string "%Y%m%d%H%M%S") slug)
                (expand-file-name "refs" org-roam-directory)))
         (org-id-overriding-file-name file)
         (id (org-id-new)))
    (org-roam-capture-
     :node (org-roam-node-create :title title)
     :templates `(("w" "web" plain
                   ,skeleton
                   :if-new (file+head ,file
                                      ,(concat ":PROPERTIES:\n"
                                               ":ID:       " id "\n"
                                               ":ROAM_REFS: " url "\n"
                                               ":END:\n"
                                               "#+title: ${title}\n"
                                               "#+todo: TODO(t) READING(r) DEVELOP(d) | DONE(D) SKIP(s)\n"
                                               "#+filetags: :lit:\n\n"
                                               "- tags :: \n"
                                               "- author :: \n\n"))
                   :immediate-finish nil
                   :unnarrowed t))
     :props '(:finalize find-file))))

;;;; Org Export

(defun viewsource/org-export-to-gfm ()
  "Export current Org buffer to GitHub-flavored Markdown using pandoc."
  (interactive)
  (let* ((input (buffer-file-name))
         (output (concat (file-name-sans-extension input) ".md")))
    (call-process "pandoc" nil nil nil
                  "-f" "org"
                  "-t" "gfm"
                  "--wrap=none"
                  "-o" output
                  input)
    (message "Exported to %s" output)))

;;;; Screenshots

(defun viewsource/org-scrot ()
  "Take a screenshot and insert a link to it in the current org buffer.
Uses flameshot for capture. The screenshot is saved in the same
directory as the org file with a timestamped filename."
  (interactive)
  (let* ((suffix (concat "_scrot-" (format-time-string "%Y%m%d%H%M%S") ".png"))
         (abs-filename (concat (buffer-file-name) suffix))
         (rel-filename (file-name-nondirectory abs-filename)))
    (call-process "flameshot" nil nil nil "gui" "-p" abs-filename)
    (insert (concat "#+CAPTION: image-caption\n"
                    "#+NAME:    fig:image-name\n"
                    "[[./" rel-filename "]]"))
    (org-display-inline-images)))

;;;; Utilities

(defun viewsource/capitalize-first-char (&optional string)
  "Capitalize only the first character of STRING.
Useful for yasnippet transformations."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(defun viewsource/ffap-js-extension ()
  "Try to find a .js file when the file at point doesn't exist.
Returns the path with .js extension if it exists, nil otherwise."
  (let* ((fname (thing-at-point 'filename t))
         (with-js (and fname (concat fname ".js"))))
    (when (and with-js (file-exists-p with-js))
      with-js)))

;;;; Region Processing

(defun viewsource/make-region-processor (command &optional working-dir success-msg error-msg)
  "Create a command that processes region through COMMAND.
WORKING-DIR sets `default-directory' for the command.
SUCCESS-MSG and ERROR-MSG are shown after processing."
  (lambda (beg end)
    (interactive "r")
    (let* ((input (buffer-substring-no-properties beg end))
           (default-directory (if working-dir
                                  (expand-file-name working-dir)
                                default-directory))
           (output (with-temp-buffer
                     (insert input)
                     (if (zerop (apply #'call-process-region
                                       (point-min) (point-max)
                                       (car command) t t nil (cdr command)))
                         (buffer-string)
                       nil))))
      (if output
          (progn
            (delete-region beg end)
            (goto-char beg)
            (insert output)
            (message "%s" (or success-msg "Region processed successfully")))
        (message "%s" (or error-msg "Processing failed - original text preserved"))))))

;;;; Link Discovery

(defun viewsource/org-roam-discover-links ()
  "Find related but unlinked org-roam notes for the current buffer.
Uses TF-IDF similarity over all notes in the slip-box."
  (interactive)
  (let* ((file (buffer-file-name))
         (buf (get-buffer-create "*link-discovery*"))
         (script (expand-file-name "~/git/viewsource/link-discovery/discover.py")))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "Searching for related notes...\n")
        (display-buffer buf '(display-buffer-in-side-window . ((side . right) (window-width . 0.4))))))
    (let ((proc (start-process "link-discovery" buf "python3" script file "--top" "10")))
      (set-process-sentinel
       proc
       (lambda (process _event)
         (when (eq (process-status process) 'exit)
           (with-current-buffer (process-buffer process)
             (let ((inhibit-read-only t))
               (goto-char (point-min))
               (when (search-forward "Searching for related notes...\n" nil t)
                 (delete-region (match-beginning 0) (match-end 0)))
               (goto-char (point-min))))))))))

(defun viewsource/org-roam-unlinked-references-section (node)
  "Show unlinked references for NODE with additional filters.

This is a copy of `org-roam-unlinked-references-section' from org-roam-mode.el,
extended because the original accepts no arguments and offers no hook for
filtering individual results.

Filters applied on top of the original behavior:
- Excludes notes in the refs/ directory (bibliography/reading notes)
- Excludes matches on #+title:, :ROAM_ALIASES:, and :ROAM_REFS: lines
- Excludes matches that appear inside an existing [[...]] link"
  (when (and (executable-find "rg")
             (org-roam-node-title node)
             (not (string-match "PCRE2 is not available"
                                (shell-command-to-string "rg --pcre2-version"))))
    (let* ((titles (cons (org-roam-node-title node)
                         (org-roam-node-aliases node)))
           (rg-command (org-roam-unlinked-references--rg-command titles))
           (results (split-string (shell-command-to-string rg-command) "\n"))
           f row col match)
      (magit-insert-section (unlinked-references)
        (magit-insert-heading "Unlinked References:")
        (dolist (line results)
          (save-match-data
            (when (string-match org-roam-unlinked-references-result-re line)
              (setq f (match-string 1 line)
                    row (string-to-number (match-string 2 line))
                    col (string-to-number (match-string 3 line))
                    match (match-string 4 line))
              (when (and match
                         (not (string-match-p "/refs/" f))
                         (not (file-equal-p (org-roam-node-file node) f))
                         (member (downcase match) (mapcar #'downcase titles)))
                (let* ((preview (org-roam-unlinked-references-preview-line f row))
                       (before-match (substring preview 0 (min (1- col) (length preview))))
                       (inside-link (and (string-match ".*\\[\\[" before-match)
                                         (not (string-match-p "\\]\\]"
                                                              (substring before-match (match-end 0)))))))
                  (unless (or inside-link
                               (let ((case-fold-search t))
                               (string-match-p "^\\s-*\\(#\\+title:\\|:ROAM_ALIASES:\\|:ROAM_REFS:\\)" preview)))
                    (magit-insert-section section (org-roam-grep-section)
                      (oset section file f)
                      (oset section row row)
                      (oset section col col)
                      (insert (propertize (format "%s:%s:%s"
                                                  (truncate-string-to-width (file-name-base f) 15 nil nil t)
                                                  row col) 'font-lock-face 'org-roam-dim)
                              " "
                              (org-roam-fontify-like-in-org-mode preview)
                              "\n"))))))))
        (insert ?\n)))))

;;;; Org inbox import

(defun viewsource/import-orgzly-inbox ()
  "Import items from Orgzly inbox.org into the 'Inbox on mobile' section of todo.org.
Requires Dropbox to be running. Clears inbox.org after import."
  (interactive)
  (unless (zerop (call-process "pgrep" nil nil nil "-x" "dropbox"))
    (user-error "Dropbox is not running"))
  (let* ((inbox-file (expand-file-name "~/Dropbox/Apps/Orgzly/inbox.org"))
         (todo-file (expand-file-name "~/org/todo.org"))
         (raw (with-temp-buffer
                (insert-file-contents inbox-file)
                (buffer-string)))
         (content (string-trim raw)))
    (if (string-empty-p content)
        (message "Orgzly inbox is leeg")
    (let* ((demoted (replace-regexp-in-string "^\\*" "**" content))
           (with-todo (with-temp-buffer
                        (insert demoted)
                        (goto-char (point-min))
                        (while (re-search-forward "^\\*\\* " nil t)
                          (unless (looking-at (regexp-opt '("TODO" "DONE" "STRT" "WAIT" "HOLD" "KILL") 'words))
                            (insert "TODO ")))
                        (buffer-string)))
           (item-count (cl-count-if (lambda (line) (string-match-p "^\\*\\* " line))
                                    (split-string with-todo "\n")))
           (buf (find-file-noselect todo-file)))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (unless (re-search-forward "^\\* .*Inbox on mobile" nil t)
            (user-error "Heading 'Inbox on mobile' niet gevonden in todo.org"))
          (end-of-line)
          (forward-line 1)
          (while (looking-at "^$") (forward-line 1))
          (insert with-todo "\n\n"))
        (save-buffer))
      (with-temp-file inbox-file (insert ""))
      (message "Geïmporteerd: %d item(s) naar todo.org" item-count)))))

(provide '+functions)
;;; +functions.el ends here
