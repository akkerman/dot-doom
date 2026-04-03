;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+functions")

; (setq doom-theme 'doom-ne)
(setq doom-theme 'gruvbox-dark-medium)
(custom-theme-set-faces!
'gruvbox-dark-medium
;; '(org-level-3 :inherit outline-3 :height 1.1)
;; '(org-level-2 :inherit outline-2 :height 1.2)
;; '(org-level-1 :inherit outline-1 :height 1.4)
;; '(org-document-title  :height 2.0 :underline nil)
)

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
                 (file+datetree "slip-box/training.org")
                (file "~/.config/doom/templates/running.org") :jump-to-captured t))
  (add-to-list 'org-capture-templates
               '("b" "Bike Journal" entry
                 (file+datetree "slip-box/training.org")
                (file "~/.config/doom/templates/bike-riding.org") :jump-to-captured t))
  (add-to-list 'org-capture-templates
               '("c" "Check-In Journal" entry
                 (file+datetree "slip-box/training.org")
                (file "~/.config/doom/templates/check-in.org") :jump-to-captured t))
  )



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
         (file "~/.config/doom/templates/aoc.org")
         :target
         (file+head "aoc/%^{year}/%^{day}.org"
                    "#+title: ${title}\n")
         :variables (("year" . "%^{year}"))
         :unnarrowed t)
        ("m" "mijn-aansluiting")
        ("md" "default" plain "%?" :target
            (file+head "mijn-aansluiting/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
            :unnarrowed t)
        ("mr" "refinement" plain
            (file "~/.config/doom/templates/refinement.org") :target
            (file+head "mijn-aansluiting/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
            :unnarrowed t)
        ("n" "Energie Onderbrekingen")
        ("nd" "default" plain "%?" :target
            (file+head "energieonderbrekingen/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
            :unnarrowed t)
    ))
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
        (setq citar-bibliography '("/home/akkerman/org/bib/library.bib" "/home/akkerman/org/bib/programming.bib" "/home/akkerman/org/bib/health.bib" ))
        (setq citar-library-paths '("/home/akkerman/Dropbox/Library" "/home/akkerman/Dropbox/Library Bought")
                citar-notes-paths '("/home/akkerman/org/slip-box/refs")
                citar-org-roam-subdir "refs"))

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

;; https://emacs.stackexchange.com/questions/2387/browser-not-opening-when-exporting-html-from-org-mode
(setq org-file-apps
      (quote
       (("\\.x?html?\\'" . browse-url))))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


;; python
(use-package pipenv
  :hook (python-mode . pipenv-mode)  ; Automatically enable pipenv in Python files
  :init
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended)) ; Hook to activate pipenv automatically

(setq python-shell-interpreter "pipenv"
      python-shell-interpreter-args "run python")

(after! lsp-pyright
  (setq lsp-pyright-venv-path (expand-file-name "~/.local/share/virtualenvs/")
        lsp-pyright-venv-directory "pipenv")) ; Adjust if your pipenv environments are elsewhere

(use-package! gptel
  :config
  (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (setq gptel-model "gpt-3.5-turbo")
  (setq gptel-default-mode 'org-mode)

  ;; AI-assisted thinking directives
  (setq gptel-directives
        '((default . "You are a large language model and a careful programmer. Respond concisely. Antwoord in het Nederlands.")

          (socratic . "You are a Socratic thinking partner. Given the text, ask exactly 3 penetrating questions that challenge the reasoning, expose hidden assumptions, or reveal blind spots. Do not answer the questions yourself. Do not restate the text. Just ask the questions, numbered 1-3. Antwoord in het Nederlands.")

          (devils-advocate . "You are a devil's advocate. Given the text, construct the strongest possible counterargument to the position presented. Be intellectually honest — argue against the position, not the person. Structure your response as: 1) The core claim you're challenging, 2) Your counterargument, 3) What evidence or perspective would be needed to resolve the tension. Antwoord in het Nederlands.")

          (connections . "You are a cross-disciplinary thinking partner. Given the text, identify 3-5 concepts, fields, or frameworks from OTHER domains that connect to the ideas presented but are NOT mentioned. For each, briefly explain the connection and why it might be a productive avenue to explore. Antwoord in het Nederlands.")

          (zettelkasten . "You are a Zettelkasten methodology expert. Evaluate this note against these principles:
1. **Atomicity**: Does this note contain exactly one core idea? If not, suggest how to split it.
2. **Title**: Does the title accurately capture the single core idea?
3. **Own words**: Is the content written in the author's own voice, or is it too close to quoting/paraphrasing?
4. **Self-contained**: Can this note be understood without reading other notes?
Be brief and actionable. Antwoord in het Nederlands.")

          (reformulate . "You are a writing clarity assistant. Given the text, help sharpen the formulation without changing the meaning or voice. Provide:
1. A tighter version of the same idea (fewer words, same meaning)
2. Flag any vague terms that could be more precise, with specific alternatives
Do NOT rewrite from scratch. Preserve the author's style and perspective. Antwoord in het Nederlands.")))

  ;; Keybindings under SPC A (AI) prefix
  (map! :leader
        (:prefix ("A" . "AI")
         :desc "GPTel send"          "a" #'gptel-send
         :desc "GPTel menu"          "m" #'gptel-menu
         :desc "Socratic questions"  "s" (cmd! (let ((gptel--system-message (alist-get 'socratic gptel-directives)))
                                                 (gptel-send (use-region-p))))
         :desc "Devil's advocate"    "d" (cmd! (let ((gptel--system-message (alist-get 'devils-advocate gptel-directives)))
                                                 (gptel-send (use-region-p))))
         :desc "Connections"         "c" (cmd! (let ((gptel--system-message (alist-get 'connections gptel-directives)))
                                                 (gptel-send (use-region-p))))
         :desc "Zettelkasten review" "z" (cmd! (let ((gptel--system-message (alist-get 'zettelkasten gptel-directives)))
                                                 (gptel-send (use-region-p))))
         :desc "Reformulate"         "r" (cmd! (let ((gptel--system-message (alist-get 'reformulate gptel-directives)))
                                                 (gptel-send (use-region-p))))
         :desc "Discover links"      "l" #'viewsource/org-roam-discover-links)))


(add-hook 'find-file-at-point-functions #'viewsource/ffap-js-extension)

;; Jira integration
(map! :leader
      :desc "Jira URLs to org" "n j"
      (viewsource/make-region-processor
       '("python" "extract.py")
       "~/git/viewsource/jira.org/"
       "Jira URLs converted to org-mode"
       "Failed to convert Jira URLs - original text preserved"))

;; Run template post processing
(map! :leader
      :desc "Enrich Run/Bike template with Strava data" "n T"
      (viewsource/make-region-processor
       '("python" "-m" "org.generate")
       "~/git/viewsource/hardloop/"
       "Template enriched with Strava data"
       "Failed to enrich template with Strava data - original text preserved"))
