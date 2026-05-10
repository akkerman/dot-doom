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
  (setq ispell-dictionary "nl")
  (add-hook 'org-mode-hook #'flyspell-mode)
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



(setq org-roam-directory (expand-file-name "~/org/slip-box"))
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
              #'viewsource/org-roam-unlinked-references-section))
    (map! :leader
          :desc "Org-roam node from URL" "n r w" #'viewsource/org-roam-node-from-url)
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
(map! :leader
      :desc "Import Orgzly inbox" "n i" #'viewsource/import-orgzly-inbox)

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

;; Dired preview
(use-package! dired-preview
  :hook (dired-mode . dired-preview-mode))

;; Javascript development

(use-package! coverlay
  :hook (js2-mode . coverlay-minor-mode)
  :config
  (defun viewsource/coverlay-load-file (lcov-file)
    (interactive (list (read-file-name "Lcov file: " (projectile-project-root))))
    (setq coverlay:base-path
          (expand-file-name (file-name-directory (directory-file-name (file-name-directory lcov-file)))))
    (coverlay-load-file lcov-file))
  (setq coverlay:mark-tested-lines nil)
  (setq coverlay:untested-line-background-color "orange red")
  (map! :leader
        (:prefix ("c v" . "coverage")
         :desc "Load lcov file"    "l" #'viewsource/coverlay-load-file
         :desc "Toggle overlays"   "t" #'coverlay-toggle-overlays
         :desc "Display stats"     "s" #'coverlay-display-stats)))

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

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(qwen3.6:35b-a3b gemma4:26b-a4b-it-q4_K_M))

  ;; AI-assisted thinking directives
  (setq gptel-directives
        '((default . "Je bent een behulpzame assistent en zorgvuldige programmeur. Antwoord beknopt. Antwoord in dezelfde taal als de invoer: Nederlands voor Nederlandse tekst, Engels voor Engelse tekst.")

          (socratic . "Je bent een Socratische gesprekspartner. Stel precies 3 doordringende vragen die de redenering uitdagen, verborgen aannames blootleggen of blinde vlekken onthullen. Beantwoord de vragen niet zelf. Herhaal de tekst niet. Geef alleen de vragen, genummerd 1-3. Antwoord in dezelfde taal als de invoer: Nederlands voor Nederlandse tekst, Engels voor Engelse tekst.")

          (devils-advocate . "Je bent advocaat van de duivel. Formuleer het sterkst mogelijke tegenargument op de gepresenteerde stelling. Wees intellectueel eerlijk — argumenteer tegen de positie, niet de persoon. Structureer je antwoord als: 1) De kernstelling die je aanvecht, 2) Jouw tegenargument, 3) Welk bewijs of perspectief nodig is om de spanning op te lossen. Antwoord in dezelfde taal als de invoer: Nederlands voor Nederlandse tekst, Engels voor Engelse tekst.")

          (connections . "Je bent een interdisciplinaire denkpartner. Identificeer 3-5 concepten, vakgebieden of frameworks uit ANDERE domeinen die verbinding hebben met de gepresenteerde ideeën maar NIET worden genoemd. Leg voor elk kort de verbinding uit en waarom het een productieve richting kan zijn. Antwoord in dezelfde taal als de invoer: Nederlands voor Nederlandse tekst, Engels voor Engelse tekst.")

          (zettelkasten . "Je bent een expert in de Zettelkasten-methode. Beoordeel deze notitie op:
1. **Atomiciteit**: Bevat deze notitie precies één kernidee? Zo niet, stel voor hoe te splitsen.
2. **Titel**: Dekt de titel het ene kernidee accuraat?
3. **Eigen woorden**: Is de inhoud in de stem van de auteur, of te dicht bij citeren/parafraseren?
4. **Zelfstandig**: Is de notitie te begrijpen zonder andere notities te lezen?
Wees beknopt en concreet. Antwoord in dezelfde taal als de invoer: Nederlands voor Nederlandse tekst, Engels voor Engelse tekst.")

          (reformulate . "Je bent een schrijfassistent voor helderheid. Verscherp de formulering zonder de betekenis of stem te veranderen:
1. Een compactere versie van hetzelfde idee (minder woorden, zelfde betekenis)
2. Markeer vage termen die preciezer kunnen, met concrete alternatieven
Herschrijf NIET vanaf nul. Behoud de stijl en het perspectief van de auteur. Antwoord in dezelfde taal als de invoer: Nederlands voor Nederlandse tekst, Engels voor Engelse tekst.")

          (factcheck . "Je bent een fact checker en bekend met de zettelkasten methode. Is deze notitie feitelijk correct? Kan er nog iets worden toegevoegd met behoud van atomicity? Antwoord in dezelfde taal als de invoer: Nederlands voor Nederlandse tekst, Engels voor Engelse tekst.")
  ))

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
         :desc "Fact check"          "f" (cmd! (let ((gptel--system-message (alist-get 'factcheck gptel-directives)))
                                                 (gptel-send (use-region-p))))
         :desc "Training reflectie"  "T" #'viewsource/training-reflect
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
