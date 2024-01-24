;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

; (setq doom-theme 'doom-one)
(setq doom-theme 'gruvbox-dark-medium)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(after! org
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-tags-column 75)
  (add-to-list 'org-capture-templates
               '("s" "Survival run Journal" entry
                 (file+datetree "slip-box/survival.org")
                 (file "~/.config/doom/survival.template.org")))
  (add-to-list 'org-capture-templates
               '("r" "Run Journal" entry
                 (file+datetree "slip-box/running.org")
                (file "~/.config/doom/running.template.org"))))

(setq org-roam-directory "~/org/slip-box")
(after! org-roam
    (setq org-roam-capture-templates '(
        ("d" "default" plain "%?" :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n") :unnarrowed t)
        ("m" "mijn-aansluiting" plain "%?" :target (file+head "mijn-aansluiting/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n") :unnarrowed t)
        ("r" "refinement" plain (file "~/.config/doom/refinement.template.org") :target (file+head "mijn-aansluiting/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n") :unnarrowed t)))
    (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section)))


(defun viewsource/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor}. (${date year}). ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain (file "~/.config/doom/reference.template.org") :if-new
                            (file+head "refs/${citekey}.org"
                                       ":PROPERTIES:
:ROAM_REFS: @${citekey}
:END:
#+title: ${title}\n")
                         :immediate-finish t
                         :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))

;; Take screenshot
(defun viewsource/org-scrot ()
  "Take a screenshot into a time stamped unique-named file in the
   same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq suffix (concat
                  "_scrot-"
                  (format-time-string "%Y%m%d%H%M%S") ".png"))
  (setq absFilename (concat (buffer-file-name) suffix))
  (setq relFilename (file-name-nondirectory absFilename))
  (call-process "flameshot" nil nil nil "gui" "-p" absFilename)
  (insert (concat "#+CAPTION: image-caption\n" "#+NAME:    fig:image-name\n" "[[./" relFilename "]]"))
  (org-display-inline-images))

;; Mode line
(setq doom-modeline-vcs-max-length 100)

;; Projectile
(setq projectile-project-search-path '("~/git/viewsource" "~/git/dsplatform" "~/git/creetion" "~/.config"))
(setq ob-mermaid-cli-path "/home/akkerman/org/node_modules/.bin/mmdc")
