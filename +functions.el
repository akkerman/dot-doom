;;; +functions.el --- Custom functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom functions for viewsource.nl Doom Emacs configuration.
;; All functions use the `viewsource/' namespace prefix.

;;; Code:

;; Silence byte-compiler warnings for external functions
(declare-function citar-select-ref "citar")
(declare-function citar--format-entry-no-widths "citar")
(declare-function org-roam-capture- "org-roam-capture")
(declare-function org-roam-node-create "org-roam-node")
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

(provide '+functions)
;;; +functions.el ends here
