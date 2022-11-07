;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Marcel Akkerman"
      user-mail-address "akkerman@viewsource.nl")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'gruvbox-dark-medium)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Org-Roam
(setq org-roam-directory "~/org/slip-box")

;; (setq
;;       bibtex-completion-pdf-field "file"
;;       bibtex-completion-bibliography '("~/org/bib/library.bib")
;;       bibtex-completion-library-path '("~/Dropbox/Library/")
;;       bibtex-completion-notes-path '("~/org/slip-box/refs")
;;       )

(setq org-roam-mode-section-functions
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))

(defun viewsource/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor}. (${date year}). ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "refs/${citekey}.org"
                                       ":PROPERTIES:
:ROAM_REFS: @${citekey}
:END:
#+title: ${title}
#+todo: READING(r) DEVELOP(d) | DONE(D) SKIP(s)
#+setupfile: theme-readtheorg.setup
#+filetags: :lit:
#+startup: overview\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))


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
(setq projectile-project-search-path '("~/git/" "~/git/dsplatform/"))

(setq ob-mermaid-cli-path "/home/akkerman/org/node_modules/.bin/mmdc")



;; https://github.com/hlissner/doom-emacs/tree/develop/modules/tools/biblio
;; https://github.com/bdarcus/citar


;; (setq! bibtex-completion-bibliography '("/home/akkerman/org/bib/library.bib"))
;; (setq! bibtex-completion-library-path '("/home/akkerman/Dropbox/Library" "/home/akkerman/Dropbox/Library Bought")
;;        bibtex-completion-notes-path "/home/akkerman/org/slip-box/refs")
;; 

(setq! citar-bibliography '("/home/akkerman/org/bib/library.bib"))
(setq! citar-library-paths '("/home/akkerman/Dropbox/Library" "/home/akkerman/Dropbox/Library Bought")
       citar-notes-paths '("/home/akkerman/org/slip-box/refs"))


;; fixing blank lines in org files
(defun unpackaged/org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))




(after! org
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-tags-column 75)
  (add-to-list 'org-capture-templates
               '("s" "Survival run Journal" entry
                 (file+datetree "slip-box/survival.org")
                 (file "~/.doom.d/survival.template.org")))
  (add-to-list 'org-capture-templates
               '("r" "Run Journal" entry
                 (file+datetree "slip-box/running.org")
                (file "~/.doom.d/running.template.org")))

  ) ; after! org


(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %<%H:%M> %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))
       ))

(use-package! org-transclusion
              :after org
              :init
;               (map!
;                :map global-map "<f12>" #'org-transclusion-add
;                :leader
;                :prefix "n"
;                :desc "Org Transclusion Mode" "t" #'org-transclusion-mode)
               )

;; automatically fix javascript errors
(setq eslint-fix-executable "eslint_d")
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

(map! :leader
      :desc "Toggle implementation and test"
      "p v" #'projectile-toggle-between-implementation-and-test)

(use-package! 'org-gcal)
(setq org-gcal-client-id "902740636832-kpb8amtvh453m2oo2p2t9lr7ml3pic0c.apps.googleusercontent.com"
      org-gcal-client-secret "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"
      org-gcal-fetch-file-alist '(("marcel.akkerman@gmail.com" .  "~/org/gcal.org")
                                  ))

(setq org-agenda-start-on-weekday 1)

(setq org-latex-toc-command "\\tableofcontents \\clearpage")
