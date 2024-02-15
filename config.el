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
                 (file "~/.config/doom/templates/survival.org")))
  (add-to-list 'org-capture-templates
               '("r" "Run Journal" entry
                 (file+datetree "slip-box/running.org")
                (file "~/.config/doom/templates/running.org"))))

(setq org-roam-directory "~/org/slip-box")
(after! org-roam
    (setq org-roam-capture-templates '(
        ("d" "default" plain "%?" :target
            (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
            :unnarrowed t)
        ("a" "Advent of Code" plain
            (file "~/.config/doom/templates/aoc.org") :target
            (file+head "aoc/%^{year}/%^{day}.org" "#+title: ${title}\n")
            :unnarrowed t)
        ("m" "mijn-aansluiting")
        ("md" "default" plain "%?" :target
            (file+head "mijn-aansluiting/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
            :unnarrowed t)
        ("mr" "refinement" plain
            (file "~/.config/doom/templates/refinement.org") :target
            (file+head "mijn-aansluiting/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
            :unnarrowed t)))
    (setq org-roam-dailies-capture-templates '(
        ("d" "default" entry "* %<%H:%M> %?" :target
            (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
    (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
    (map! :leader
          (:prefix ("n r u" . "ORUI")
           :desc "add to local"
           "a" #'org-roam-ui-add-to-local-graph
           :desc "toggle follow"
           "f" #'org-roam-ui-follow-mode
           :desc "node local graph"
           "l" #'org-roam-ui-node-local
           :desc "ui open"
           "o" #'org-roam-ui-open
           :desc "togle node in graph"
           "t" #'org-roam-ui-change-local-graph
           :desc "node zoom"
           "z" #'org-roam-ui-node-zoom)))





;; citations
(after! citar
        (setq citar-bibliography '("/home/akkerman/org/bib/library.bib"))
        (setq citar-library-paths '("/home/akkerman/Dropbox/Library" "/home/akkerman/Dropbox/Library Bought")
                citar-notes-paths '("/home/akkerman/org/slip-box/refs")
                citar-org-roam-subdir "refs"))

(defun viewsource/org-roam-node-from-cite (keys-entries)
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

;; Saved macros
(fset 'BLI\ header
   (kmacro [?0 ?y ?t ?  ?m ?m ?? ?\C-r ?\" return ?d ?d ?\' ?m ?p ?k ?  ?m ?h ?k] 0 "%d"))
