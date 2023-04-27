;;; config.el --- Org-extra configuration File for Spacemacs
;; Time-stamp: <2023-04-22 Sat 03:20 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs|use-package-add-hook org
  :pre-init
  (setq org-directory "~/org"
        org-default-notes-file "~/org/notes.org")
  (setq org-modules '(ol-bbdb ol-bibtex org-crypt
                      ol-docview ol-doi ol-eww ol-gnus
                      org-habit ol-info ol-irc ol-mhe
                      org-mouse org-protocol ol-rmail
                      ol-w3m ol-elisp-symbol ol-git-link
                      ol-man org-toc org-id))
  :post-init
  (add-hook 'after-save-hook #'org-redisplay-inline-images)
  (add-hook 'org-mode-hook #'toc-org-mode)

  :post-config
  (setq org-indirect-buffer-display 'current-window)

  (setq org-file-apps
        '(("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . system)
          ("\\.png\\'" . system)
          ("\\.jpg\\'" . system)
          ("\\.jpeg\\'" . system)
          ("\\.bmp\\'" . system)
          ("\\.svg\\'" . system)
          (directory . emacs)
          (auto-mode . emacs)))

  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame)))

  (setq org-export-backends '(ascii beamer html latex man md odt org texinfo)
        org-export-use-babel nil
        org-export-with-sub-superscripts '{})

  (setq org-edit-src-turn-on-auto-save t)

  (setq org-global-properties
        '(("POMODORO_ALL" . "0 1 2 3 4 5")
          ("SCORE_ALL" . "0 1 2 3 4 5")))

  (setq org-format-latex-header
        "\\documentclass{article}\12\\usepackage[usenames]{color}\12[PACKAGES]\12[DEFAULT-PACKAGES]\12% [removed] For displaying tikz pictures in latex fragments\12% \\usepackage{tikz}\12% \\usetikzlibrary{shadings}\12% For displaying simplified chinese characters in latex fragments\12\\usepackage{bm}\12\\usepackage{fontspec}\12\\setmainfont{Noto Serif CJK SC}\12\\pagestyle{empty}             % do not remove\12% The settings below are copied from fullpage.sty\12\\setlength{\\textwidth}{\\paperwidth}\12\\addtolength{\\textwidth}{-3cm}\12\\setlength{\\oddsidemargin}{1.5cm}\12\\addtolength{\\oddsidemargin}{-2.54cm}\12\\setlength{\\evensidemargin}{\\oddsidemargin}\12\\setlength{\\textheight}{\\paperheight}\12\\addtolength{\\textheight}{-\\headheight}\12\\addtolength{\\textheight}{-\\headsep}\12\\addtolength{\\textheight}{-\\footskip}\12\\addtolength{\\textheight}{-3cm}\12\\setlength{\\topmargin}{1.5cm}\12\\addtolength{\\topmargin}{-2.54cm}\12\\DeclareMathOperator*{\\argmax}{argmax}\\DeclareMathOperator*{\\argmin}{argmin}")

  ;; (setq org-format-latex-header
  ;;       "\\documentclass{article}\12\\usepackage[usenames]{color}\12[PACKAGES]\12[DEFAULT-PACKAGES]\12% [removed] For displaying tikz pictures in latex fragments\12% \\usepackage{tikz}\12% \\usetikzlibrary{shadings}\12% For displaying simplified chinese characters in latex fragments\12\\usepackage{fontspec}\12\\setmainfont{Noto Serif CJK SC}\12\\pagestyle{empty}             % do not remove\12% The settings below are copied from fullpage.sty\12\\setlength{\\textwidth}{\\paperwidth}\12\\addtolength{\\textwidth}{-3cm}\12\\setlength{\\oddsidemargin}{1.5cm}\12\\addtolength{\\oddsidemargin}{-2.54cm}\12\\setlength{\\evensidemargin}{\\oddsidemargin}\12\\setlength{\\textheight}{\\paperheight}\12\\addtolength{\\textheight}{-\\headheight}\12\\addtolength{\\textheight}{-\\headsep}\12\\addtolength{\\textheight}{-\\footskip}\12\\addtolength{\\textheight}{-3cm}\12\\setlength{\\topmargin}{1.5cm}\12\\addtolength{\\topmargin}{-2.54cm}")

  (setq org-format-latex-options
        '(:foreground default
          :background default
          :scale 2.0
          :html-foreground "Black"
          :html-background "Transparent"
          :html-scale 2.0
          :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  (setq org-latex-classes
        '(("beamer" "\\documentclass[presentation]{beamer}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("elegantpaper" "\\documentclass[a4paper,11pt,bibtex]{elegantpaper}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("elegantnote" "\\documentclass[14pt,blue,screen]{elegantnote}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("article" "\\documentclass[11pt]{article}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("report" "\\documentclass[11pt]{report}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("book" "\\documentclass[11pt]{book}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

  (setq org-latex-compiler "xelatex")

  (setq org-latex-listings 'minted
        org-latex-minted-langs '((jupyter-python "python")
                                 (python "python")
                                 (emacs-lisp "common-lisp")
                                 (cc "c++")
                                 (cmake "cmake")
                                 (cperl "perl")
                                 (shell-script "bash")
                                 (caml "ocaml")
                                 (c "c")
                                 (json "json")
                                 (javascript "js")
                                 (html "html")
                                 (css "css")
                                 (matlab "matlab")
                                 (bash "bash")
                                 (sql "sql")
                                 (sqlite "sqlite3")
                                 (common-lisp "common-lisp")
                                 (dockerfile "dockerfile")
                                 (yaml "yaml")
                                 (graphviz-dot "dot"))
        org-latex-minted-options '(("linenos" "true")
                                   ("mathescape" "")
                                   ("breaklines" "")
                                   ("fontsize" "\\footnotesize")
                                   ("frame" "lines"))
        org-latex-packages-alist '(("newfloat" "minted" nil))
        org-latex-pdf-process '("latexmk -f -pdf -%latex -interaction=nonstopmode -shell-escape -output-directory=%o %f" "latexmk -c %f")
        org-latex-src-block-backend 'minted)

  (setq org-preview-latex-default-process 'imagemagick
        org-preview-latex-process-alist
        '((dvipng
           :programs ("latex" "dvipng")
           :description "dvi > png"
           :message "you need to install the programs: latex and dvipng."
           :image-input-type "dvi"
           :image-output-type "png"
           :image-size-adjust (1.0 . 1.0)
           :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
           :image-converter ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
          (dvisvgm
           :programs ("xelatex" "dvisvgm")
           :description "xdv > svg"
           :message "you need to install the programs: xelatex and dvisvgm."
           :image-input-type "xdv"
           :image-output-type "svg"
           :image-size-adjust (1.7 . 1.5)
           :latex-compiler ("xelatex -shell-escape -no-pdf -interaction nonstopmode -output-directory %o %f")
           :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick
           :programs ("xelatex" "convert")
           :description "pdf > png"
           :message "you need to install the programs: xelatex and imagemagick."
           :image-input-type "pdf"
           :image-output-type "png"
           :image-size-adjust (1.0 . 1.0)
           :latex-compiler ("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
           :image-converter ("MAGICK_CONFIGURE_PATH='$HOME/.config/ImageMagick/:/etc/ImageMagick-6/' convert -density %D -trim -antialias %f -quality 100 -colorspace RGB %O"))))

  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-redeadline 'note
        org-log-refile 'time
        org-log-reschedule 'time
        org-log-state-notes-insert-after-drawers t)

 (setq org-refile-targets '((nil :maxlevel . 4)
                            (org-agenda-files :maxlevel . 4))
       org-refile-use-outline-path 'title)

 (setq org-reverse-note-order t)

 (setq org-src-ask-before-returning-to-edit-buffer nil
       org-src-preserve-indentation t)

 (setq org-stuck-projects '("+PROJECT/-SOMEDAY-DONE" ("NEXT" "STARTED")))

 (setq org-tag-persistent-alist
       '((:startgrouptag)
         ("PROJECT" . 80) ("AREA" . 65) ("RESOURCE" . 82) ("ARCHIVE" . 90)
         (:endgrouptag)
         (:startgrouptag)
         ("CONFIDENTIAL" . 67) ("FLAGGED" . 70) ("ATTACH" . 84) ("crypt" . 88)
         (:endgrouptag)
         (:startgrouptag)
         ("glossary" . 103) ("reference" . 114) ("literature" . 108)
         ("fleeting" . 102) ("permanent" . 112) ("code" . 99)
         ("hub" . 104) ("publication" . 98) ("vocabulary" . 118)
         (:endgrouptag)
         (:startgrouptag)
         ("noexport" . 110) ("TOC" . 79) ("repeat" . 114)
         (:endgrouptag)
         (:startgrouptag)
         ("action" . 116) ("hidden" . 104) ("status" . 115)
         (:endgrouptag)))

 (setq org-todo-keywords
       '((sequence "TODO(t)" "SOMEDAY(x)" "NEXT(n)"
                   "STARTED(s!)" "WAITING(w!)" "|"
                   "DONE(d!)" "CANCELLED(c@/!)")
         (sequence "NEW(a)" "REVIEW(r!)" "|"
                   "MARK(m!)" "USELESS(u!)")))

 (setq org-treat-S-cursor-todo-selection-as-state-change nil
       org-treat-insert-todo-heading-as-state-change t)

 (setq org-use-property-inheritance "header-args\\|shebang\\|session\\|DIR\\|dir"
       org-use-tag-inheritance '("fleeting" "AREA" "RESOURCE" "ARCHIVE"
                                 "action" "status" "hidden" "publication"
                                 "code" "vocabulary" "ATTACH"))

  (setq org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t)

  (setq org-after-todo-state-change-hook
    '((lambda nil
        (when
            (equal org-state "DONE")
          (xy/org-roam-copy-todo-to-today)))
      (closure
          (t)
          nil
        (if
            (or
             (string= org-state "SOMEDAY")
             (string= org-state "TODO"))
            (org-remove-timestamp-with-keyword org-scheduled-string))
        (if
            (string= org-state "NEXT")
            (org-deadline nil "+0"))
        (if
            (string= org-state "DONE")
            (alert "WELL DONE"
                   :title "Agenda"
                   :category 'Emacs
                   :severity 'trivial))
        (if
            (string= org-state "REVIEW")
            (org-fc-type-double-init)))))

  (setq org-capture-templates
        '(("t" "Task" entry
           (id "c99c005d-0aaa-46dd-b889-f8579726aa2a")
           "* TODO %?\12:LOGBOOK:\12- Create time: %U\12- From: %a\12:END:\12- Tags:     [ add exsisting reference notes as tags]\12- See also: [ add existing literate, fleeting, and permanent notes that relates with this project ]"
           :prepend t :empty-lines 1 :clock-keep t)
          ("n" "Note" entry
           (id "eb39c457-7821-4600-85a8-e8fa76d328ab")
           "* NEW %?    :fleeting:\12:LOGBOOK:\12- Create time: %U\12- From: %a\12:END:\12- Tags:     [ add reference notes ]\12- See also: [ add fleeting and permanent notes ]"
           :prepend t :empty-lines 1 :clock-keep t)
          ("e" "English" entry
           (id "929598fb-92c7-4321-9681-43e59a4f9d9f")
           "* NEW %?\12:PROPERTIES:\12:ROAM_EXCLUDE: t\12:END:\12:LOGBOOK:\12- Create time: %U\12- From: %a\12:END:"
           :prepend t :empty-lines 1 :clock-keep t)
          ("b" "Bookmark" entry
           (id "0822a2de-0d55-432c-967d-c2b2369df980")
           "* NEW %a\12:LOGBOOK:\12- Create time: %U\12- From: %a\12:END:\12- URL: %L\12- Tags: [add reference nodes ]\12- Notes:"
           :prepend t :empty-lines 1 :clock-keep t)
          ("c" "Contacts" entry
           (id "f3c11ccd-31b0-45be-9046-42f6e6a2a7c6")
           "* %(org-contacts-template-name)\12:PROPERTIES:\12:COMPANY:\12:POSITION:\12:OCCUPATION:\12:NOTE:\12:PHONE:\12:WeChat:\12:WXWORK:\12:EMAIL: %(org-contacts-template-email)\12:ALITINGTING:\12:QQ:\12:ALIAS:\12:NICKNAME:\12:BIRTHDAY:\12:ADDRESS:\12:END:"
           :prepend t :empty-lines 1 :clock-keep t)
          ("x" "Password" entry
           (id "8c510a93-b780-4782-afbd-f61e38d42e25")
           "* %?\12:LOGBOOK:\12- Create time: %U\12- From: %a\12:END:\12- Website:\12- Username:\12- Password:\12- Tags: [add reference nodes ]\12- Description:"
           :prepend t :empty-lines 1 :clock-keep t)))

  (setq org-columns-default-format
        "%CATEGORY(Cat.) %PRIORITY(Pri.) %6TODO(State) %35ITEM(Details) %ALLTAGS(Tags) %5NUM_POMODORO(Plan){:} %6CLOCKSUM(Clock){Total} %SCORE(SCORE)")
  (setq org-confirm-babel-evaluate nil)
  (setq org-archive-save-context-info
        '(time file category todo priority itags olpath ltags))
  (setq org-clock-history-length 10
        org-clock-idle-time 15
        org-clock-in-resume t
        org-clock-in-switch-to-state "STARTED"
        org-clock-into-drawer "LOGBOOK"
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-switch-to-state "WAITING"
        org-clock-persist t
        org-clock-persist-query-save t
        org-clock-report-include-clocking-task t
        org-clock-sound t)

  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-disable-auto-save 'encrypt
        org-crypt-key "etimecowboy@gmail.com")

  (require 'org-attach)
  (require 'org-attach-git)
  (setq org-attach-archive-delete 'query
        org-attach-id-dir "data/"
        org-attach-store-link-p 'attached
        org-attach-use-inheritance t)
  ;; acctach from dired
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map
                          (kbd "C-c C-x a")
                          #'org-attach-dired-to-subtree)))
  )

(spacemacs|use-package-add-hook org-agenda
  :post-init
  (add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list)
  :post-config
  (setq org-agenda-block-separator 9473
        org-agenda-custom-commands
        '(("d" "Day Planner"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-deadline-warning-days 14)
                     (org-agenda-use-time-grid t)
                     (org-agenda-skip-scheduled-if-done t)
                     (org-agenda-skip-deadline-if-done t)
                     (org-agenda-skip-timestamp-if-done t)
                     (org-agenda-skip-archived-trees t)
                     (org-agenda-skip-comment-trees t)
                     (org-agenda-todo-list-sublevel t)
                     (org-agenda-timeline-show-empty-dates t)))
            (tags-todo "TODO<>\"NEW\"+TODO<>\"TODO\"+TODO<>\"SOMEDAY\"-SCHEDULED<=\"<+7d>\"-SCHEDULED>\"<+14d>\"-DEADLINE<=\"<+7d>\"-DEADLINE>\"<+14d>\"-repeat-appt-fc"
                       ((org-agenda-overriding-header "Pending Next Actions")
                        (org-tags-match-list-sublevels t)))
            (tags-todo "TODO=\"TODO\"-SCHEDULED-DEADLINE-repeat"
                       ((org-agenda-overriding-header "Task Inbox")
                        (org-tags-match-list-sublevels t)))
            (tags-todo "TODO=\"NEW\""
                       ((org-agenda-overriding-header "New Stuff")
                        (org-tags-match-list-sublevels t)))
            (tags-todo "SCHEDULED>=\"<+1d>\"+SCHEDULED<=\"<+7d>\"-repeat-fc"
                       ((org-agenda-overriding-header "Scheduled Tasks in 7 Days")
                        (org-tags-match-list-sublevels nil)))
            (tags-todo "TODO=\"SOMEDAY\""
                       ((org-agenda-overriding-header "Future Work")
                        (org-tags-match-list-sublevels nil))))
           nil)))
  (setq org-agenda-dim-blocked-tasks nil
        org-agenda-exporter-settings '((ps-number-of-columns 2)
                                       (ps-landscape-mode t)
                                       (org-agenda-add-entry-text-maxlines 5)
                                       (htmlize-output-type 'css))
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-sorting-strategy
        '((agenda time-up category-keep priority-down todo-state-up)
          (todo time-up category-keep priority-down todo-state-up)
          (tags time-up category-keep priority-down todo-state-up)
          (search time-up category-keep priority-down todo-state-up))
        org-agenda-todo-ignore-scheduled 'all
        org-agenda-todo-list-sublevels nil
        org-agenda-window-frame-fractions '(0.2 . 0.8)
        org-agenda-window-setup 'only-window)
  )


(spacemacs|use-package-add-hook ob
  :pre-init
  (add-to-list 'org-babel-load-languages '(sqlite . t))
  (add-to-list 'org-babel-load-languages '(latex . t))
  (add-to-list 'org-babel-load-languages '(ditaa . t))
  :post-config
  (require 'ob-sqlite)
  (require 'ob-latex)
  (require 'ob-ditaa)
  (require 'ob-plantuml)
  (ad-activate 'org-babel-execute-src-block)
  (add-hook 'org-babel-after-execute-hook #'xy/org-babel-after-execute)
  (setq org-ditaa-eps-jar-path "/opt/DitaaEps/DitaaEps.jar"
        org-ditaa-jar-path "/opt/ditaa/ditaa.jar")
  (setq org-plantuml-args '("-headless" "-DRELATIVE_INCLUDE=\".\"")
        org-plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\"")
        org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
  )


(spacemacs|use-package-add-hook org-appear
  :post-config
  (setq org-appear-autoentities t
        org-appear-autolinks 'just-brackets
        org-appear-autosubmarkers t
        org-appear-delay 0.8
        org-appear-inside-latex t)
  )


;; (spacemacs|use-package-add-hook org-attach
;;   :pre-init
;;   (setq org-attach-use-inheritance t)
;;   :post-config
;;   (require 'org-attach-git)
;;   ;; acctach from dired
;;   (add-hook 'dired-mode-hook
;;             (lambda ()
;;               (define-key dired-mode-map
;;                           (kbd "C-c C-x a")
;;                           #'org-attach-dired-to-subtree)))
;;   (setq org-attach-archive-delete 'query
;;         org-attach-id-dir "data/"
;;         org-attach-store-link-p 'attached)
;;   )

(spacemacs|use-package-add-hook org-contacts
  :pre-init
  (setq org-contacts-files '("~/org/roam/contacts.org.gpg"))
  )

(spacemacs|use-package-add-hook org-download
  :post-init
  (add-hook 'dired-mode-hook 'org-download-enable)
  :post-config
  (setq org-download-edit-cmd "krita %s"
        org-download-image-org-width 200
        org-download-method 'attach
        org-download-screenshot-method "scrot -s %s")
  )

(spacemacs|use-package-add-hook org-ref
  :post-config
  (setq org-ref-open-pdf-function
        (lambda (fpath)
          (start-process "zathura"
                         "*bibtex-zathura*" ;; was "*helm-bibtex-zathura*", changed because helm was removed
                         "/usr/bin/zathura" fpath))))

;; layer: markdown
;; (with-eval-after-load "markdown"
;;   (add-hook 'markdown-mode-hook #'toc-org-mode))
(spacemacs|use-package-add-hook markdown
  :post-init
  (add-hook 'markdown-mode-hook #'toc-org-mode))

(spacemacs|use-package-add-hook org-roam
  :post-init
  (setq org-roam-dailies-directory "~/org/dailies"
        org-roam-db-location "~/org/org-roam.db"
        org-roam-directory "~/org/roam")
  (add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list)
  :post-config
  (setq org-roam-v2-ack t
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-completion-everywhere t
        org-roam-list-files-commands '(rg)
        org-roam-mode-sections '(org-roam-backlinks-section
                                 org-roam-reflinks-section
                                 org-roam-unlinked-references-section)
        org-roam-protocol-store-links t)
  (org-roam-db-autosync-mode 1)

  (setq org-roam-capture-ref-templates
        '(("a" "annote" plain "%?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}\12#+filetags: reference PROJECT\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨Create from: %a\12- 🎛️Areas: [ add areas-of-responsibility ]\12- 🏷️Tags:  [ add reference notes ]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* 🚀Abstract\12\12* 📚References\12:PROPERTIES:\12:ROAM_EXCLUDE: t\12:END:\12\12[ If necessary, use org-web-tools-archive-attach to download a compressed copy. ]\12\12- %a\12\12* 📔Outcomes\12\12[ Put reading notes here, which will be archived as permanent notes. ]\12\12* 🔧Tasks\12\12** TODO Quickly scan useful contents of %a")
           :immediate-finish t
           :jump-to-captured t
           :empty-lines 1)))

  (setq org-roam-capture-templates
        '(("d" "fleeting (default)"
           entry "* NEW %^{title}    :fleeting:\12:LOGBOOK:\12- Create time: %U\12- From: %a\12- Tags: [ add reference notes ]\12- See also: [ add fleeting and permanent notes ]\12- Areas: [ add areas-of-responsibility ]\12:END:"
           :target (file "~/org/roam/note_inbox.org")
           :prepend t
           :empty-lines 1)
          ("l" "literature"
           plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\12#+filetags: literature\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨️Create from: %a\12- 🎛️Areas: [ add areas-of-responsibility if applicable]\12- 🏷️Tags:  [ add reference notes ]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* 🧠Thought\12\12* 📚References")
           :empty-lines 1)
          ("p" "permanent"
           plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\12#+filetags: permanent\12# Time-stamp:  %U\12- 🚥Status: [[roam:INCOMING]]\12- 🏷️Tags: [ add reference notes ]\12- 🎛️Areas: [ add areas-of-responsibility ]\12\12* 🧠Thought\12\12* 📚References\12\12* 🌏Context        :noexport:\12\12** ❔Why noting\12\12** 🧭Compass\12\12- 🔼North: (Where X comes from, parent nodes)\12- ◀️West: (What's similar to X, friend nodes)\12- ▶️East: (What's opposite of X, friend nodes)\12- 🔽South: (Where X leads to, child nodes)\12\12** 💓Feelings\12\12* 📜Change Logs        :noexport:\12\12- %U Note was create from %a")
           :prepend t
           :empty-lines 1 :
           unnarrowed t)
          ("r" "reference"
           plain "%?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}\12#+filetags: reference\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨️Create from: %a\12- 🎛️Areas: [ add areas-of-responsibility ]\12- 🏷️Tags:  [ add reference notes ]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* 🚀Abstract\12\12* 📚References\12:PROPERTIES:\12:ROAM_EXCLUDE: t\12:END:\12\12[ If necessary, use org-web-tools-archive-attach to download a compressed copy. ]\12\12- %a\12\12* 📓Outcomes\12\12[ Put reading notes here, which will be archived as permanent notes. ]")
           :jump-to-captured t :empty-lines 1)
          ("g" "glossary"
           plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\12#+filetags: glossary\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨Create from: %a\12- 🏷️Tags:  [ add reference notes ]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* ℹ️About ${title}\12\12[ basic information ]\12\12* [[roam:${title} concepts and terminologies]]\12\12* 📚References")
           :empty-lines 1
           :unnarrowed t)
          ("h" "hub"
           plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\12#+filetags: hub\12# Time-stamp:  %U\12- Create time: %U\12- From: %a")
           :empty-lines 1
           :unnarrowed t nil nil)
          ("j" "project"
           plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\12#+category: ${title}\12#+filetags: PROJECT\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨️Create from: %a\12- 🎛️Areas: [ add areas-of-responsibility ]\12- 🏷️Tags:  [ add reference notes ]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* 🥅Goal\12\12[ Describe WHAT YOU REALLY WANT if the project was completed ]\12\12* 🔧Tasks\12:PROPERTIES:\12:ROAM_EXCLUDE: t\12:END:\12\12** TODO Define the goal of the project.\12** TODO Add areas-of-responsibility, tags, and exsisting notes related to this project.\12** TODO Set a project deadline.\12** Version 1.0\12*** TODO Finish v1.0")
           :prepend t
           :clock-keep t
           :unnarrowed t)
          ("v" "vocabulary"
           plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\12#+filetags: :vocabulary:fleeting:\12# Time-stamp:  %U\12- Create time: %U\12- From: %a\12- Tags: [[roam:English Language Inbox]]")
           :empty-lines 1
           :unnarrowed t nil nil)
          ("s" "software"
           plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\12#+filetags: glossary\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨Create from: %a\12- 🏷️Tags:  [[roam:software]]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* ℹ️About ${title}\12\12[ Logo image ]\12[ TL;DR - basic information about ${title} ]\12\12* [[roam:${title} concepts and terminologies]]\12\12* [[roam:${title} maintenance work]]\12** [[roam:Install ${title} on Ubuntu]]\12** [[roam:${title} configuration]]\12** [[roam:${title} extensions and plugins]]\12\12* [[roam:${title} useful resources]]\12\12* [[roam:${title} tips and tricks]]")
           :unnarrowed t)
          ("c" "code"
           plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\12#+filetags: code\12#+PROPERTY: header-args:lang :tangle \"/path/to/tangled_source_code\" :mkdirp no\12#+auto_tangle: t\12# Time-stamp:  %U\12- Create time: %U\12- Origin: %a\12- Tags:  [ add reference notes ]\12- See also: [ add fleeting and permanent notes ]\12\12* Description\12\12* Code\12\12* Tests\12\12* References")
           :prepend t
           :empty-lines 1
           :clock-keep t
           :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
        '(("d" "default"
           entry "** %U %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\12* Mind Path\12\12Track my mind of the day to help myself focus on the main tasks.\12\12* Tasks\12:PROPERTIES:\12:ROAM_EXCLUDE: t\12:END:")
           :empty-lines 1
           :unnarrowed t)))
  )

(spacemacs|use-package-add-hook org-roam-ui
  :post-init
  (spacemacs|diminish org-roam-ui-mode " ⓤ" " u")
  (spacemacs|diminish org-roam-ui-follow-mode))

(with-eval-after-load 'alert
  (setq alert-default-style 'libnotify))
(spacemacs|use-package-add-hook org-wild-notifier
  :post-config
  (setq org-wild-notifier-alert-time '(25 15 10 5 3 1)
        org-wild-notifier-keyword-blacklist '("DONE" "CANCELLED" "MARK" "USELESS")
        org-wild-notifier-keyword-whitelist nil
        org-wild-notifier-tags-blacklist '("ARCHIVE")))
