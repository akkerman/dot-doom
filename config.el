;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

; (setq doom-theme 'doom-one)
(setq doom-theme 'gruvbox-dark-medium)
(custom-theme-set-faces!
'gruvbox-dark-medium
;; '(org-level-3 :inherit outline-3 :height 1.1)
'(org-level-2 :inherit outline-2 :height 1.2)
'(org-level-1 :inherit outline-1 :height 1.4)
'(org-document-title  :height 2.0 :underline nil))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; calendar
(setq calendar-week-start-day 1)


;; https://www.emacswiki.org/emacs/CalendarWeekNumbers
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(after! org
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-start-on-weekday 1)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-tags-column 75)
  (add-to-list 'org-capture-templates
                '("t" "Personal todo" entry
                (file+headline +org-capture-todo-file "Inbox")
                "* TODO %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d> %<%a> %<%H:%M>]\n:END:\n%i\n%a" :prepend t))
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
            (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\ndescription\nwhy this note")
            :unnarrowed t)
        ("p" "person" plain "%?" :target
            (file+head "persons/${slug}.org" "#+title:  ${title}\n#+filetags: :person:")
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
           :desc "ui open"
           "o" #'org-roam-ui-open
           :desc "toggle follow"
           "f" #'org-roam-ui-follow-mode
           :desc "node zoom"
           "z" #'org-roam-ui-node-zoom
           :desc "node local graph"
           "l" #'org-roam-ui-node-local
           :desc "add to local"
           "a" #'org-roam-ui-add-to-local-graph
           :desc "remove from local"
           "r" #'org-roam-ui-remove-from-local-graph
           :desc "toggle node in graph"
           "t" #'org-roam-ui-change-local-graph
           )))

;; citations
(after! citar
        (setq citar-bibliography '("/home/akkerman/org/bib/library.bib" "/home/akkerman/org/bib/programming.bib" ))
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

;; Javascript development

;; automatically fix javascript errors
(setq eslint-fix-executable "eslint_d")
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

(map! :leader
      :desc "Toggle impl and test"
      "p v"
        #'projectile-toggle-between-implementation-and-test)

(map! :leader
      :desc "Split, toggle impl and test"
      "w a"
      (cmd!
       (evil-window-vsplit)
       (projectile-toggle-between-implementation-and-test)))

(map! :leader
      :desc "Split, toggle impl and test"
      "w i"
      (cmd!
       (evil-window-split)
       (evil-edit "index.js")))

;; Saved macros
(fset 'BLI\ header
   (kmacro [?0 ?y ?t ?  ?m ?m ?? ?\C-r ?\" return ?d ?d ?\' ?m ?p ?k ?  ?m ?h ?k] 0 "%d"))

;; related to yasnippets
;; https://emacs.stackexchange.com/questions/12613/convert-the-first-character-to-uppercase-capital-letter-using-yasnippet
(defun my/capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))
