(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command-style
   '(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %(output-dir) %S%(PDFout)")))
 '(alert-default-style 'libnotify)
 '(bibtex-autokey-name-year-separator "-")
 '(bibtex-autokey-titleword-separator "-")
 '(bibtex-autokey-titlewords 2)
 '(bibtex-autokey-titlewords-stretch 1)
 '(bibtex-autokey-year-length 4)
 '(bibtex-autokey-year-title-separator "-")
 '(bibtex-completion-additional-search-fields '(keywords))
 '(bibtex-completion-bibliography '("~/org/bib/all.bib"))
 '(bibtex-completion-display-formats
   '((article . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
     (inbook . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
     (incollection . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (t . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))
 '(bibtex-completion-library-path '("~/doc/"))
 '(bibtex-completion-notes-path "~/org/roam/")
 '(bibtex-completion-notes-template-multiple-files
   "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: 

See [[cite:&${=key=}]]
")
 '(bibtex-completion-pdf-open-function
   '(closure
     (t)
     (fpath)
     (call-process "open" nil 0 nil fpath)))
 '(company-emoji-insert-unicode t t)
 '(consult-preview-key nil)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "dad40020beea412623b04507a4c185079bff4dcea20a93d8f8451acb6afc8358" "a0415d8fc6aeec455376f0cbcc1bee5f8c408295d1c2b9a1336db6947b89dd98" "d600c677f1777c1e4bfb066529b5b73c0179d0499dd4ffa3f599a0fb0cfbd501" default))
 '(global-hardhat-mode t)
 '(hardhat-basename-protected-regexps
   '("~\\'" "\\.lock\\'" "\\.ix\\'" "\\`test\\.out\\'" "-autoloads\\.el\\'" "\\`Desktop\\.ini\\'" "\\`META\\.yml\\'" "\\`MYMETA\\.yml\\'" "\\`TAGS\\'" "\\`Thumbs\\.db\\'" "\\`\\.dropbox\\'" "\\`\\.dropbox\\.cache\\'" "\\`\\.emacs\\.desktop\\'" "\\`\\.emacs\\.desktop\\.lock\\'" "\\.orig\\'" "\\.rej\\'" "\\.bak\\'"))
 '(hardhat-fullpath-protected-regexps
   '("~/\\.emacs\\.d/elpa/" "~/\\.cpan/" "~/\\.cabal/" "~/perl5/perlbrew/" "~/\\.npm/" "~/\\.virtualenv/" "~/\\.virthualenv/" "~/\\.rvm/" "/[._]build/" "/\\.bzr/" "/\\.coverage/" "/\\.git/" "/\\.hg/" "/\\.rspec/" "/\\.sass-cache/" "/\\.svn/" "/_MTN/" "/_darcs/" "/CVS/" "/pm_to_blib/" "/RCS/" "/SCCS/" "/blib/" "/test_output/" "~/\\.emacs\\.d/\\.cask/" "~/\\.cask/" "~/\\.cargo/" "~/\\.conda/" "~/\\.docker/" "~/\\.local/" "~/\\.rustup/" "~/\\.ssh/" "~/bin/"))
 '(ispell-program-name "hunspell")
 '(lsp-keymap-prefix "M-#")
 '(modus-themes-bold-constructs t)
 '(modus-themes-diffs 'desaturated)
 '(modus-themes-fringes 'subtle)
 '(modus-themes-headings
   '((1 background overline rainbow)
     (2 background rainbow)
     (3 no-bold rainbow)
     (t no-bold)))
 '(modus-themes-italic-constructs t)
 '(modus-themes-mode-line '(nil accented))
 '(modus-themes-org-agenda '((header-block variable-pitch) (header-date bold-today)))
 '(modus-themes-org-blocks nil)
 '(modus-themes-paren-match '(bold intense))
 '(modus-themes-prompts '(intense bold))
 '(modus-themes-region '(bg-only))
 '(modus-themes-scale-headings t)
 '(modus-themes-subtle-line-numbers t)
 '(modus-themes-syntax '(yellow-comments green-strings))
 '(modus-themes-tabs-accented t)
 '(modus-themes-variable-pitch-headings t)
 '(modus-themes-variable-pitch-ui t)
 '(org-after-todo-state-change-hook
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
          (alert "WELL DONE" :title "Agenda" :category 'Emacs :severity 'trivial))
      (if
          (string= org-state "REVIEW")
          (org-fc-type-double-init)))))
 '(org-agenda-block-separator 9473)
 '(org-agenda-custom-commands
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
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-exporter-settings
   '((ps-number-of-columns 2)
     (ps-landscape-mode t)
     (org-agenda-add-entry-text-maxlines 5)
     (htmlize-output-type 'css)))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   '((agenda time-up category-keep priority-down todo-state-up)
     (todo time-up category-keep priority-down todo-state-up)
     (tags time-up category-keep priority-down todo-state-up)
     (search time-up category-keep priority-down todo-state-up)))
 '(org-agenda-todo-ignore-scheduled 'all)
 '(org-agenda-todo-list-sublevels nil)
 '(org-agenda-window-frame-fractions '(0.2 . 0.8))
 '(org-agenda-window-setup 'only-window)
 '(org-appear-autoentities t)
 '(org-appear-autolinks 'just-brackets t)
 '(org-appear-autosubmarkers t t)
 '(org-appear-delay 0.8)
 '(org-appear-inside-latex t)
 '(org-archive-save-context-info '(time file category todo priority itags olpath ltags))
 '(org-attach-archive-delete 'query)
 '(org-attach-id-dir "data/")
 '(org-attach-store-link-p 'attached)
 '(org-capture-templates
   '(("t" "Task" entry
      (id "c99c005d-0aaa-46dd-b889-f8579726aa2a")
      "* TODO %^{Task}
:LOGBOOK:
- Create time: %U
- From: %a
:END:
- Tags:     [ add exsisting reference notes as tags]
- See also: [ add existing literate, fleeting, and permanent notes that relates with this project ]" :prepend t :empty-lines 1 :clock-keep t)
     ("n" "Note" entry
      (id "eb39c457-7821-4600-85a8-e8fa76d328ab")
      "* NEW %^{Title}    :fleeting:
:LOGBOOK:
- Create time: %U
- From: %a
:END:
- Tags:     [ add reference notes ]
- See also: [ add fleeting and permanent notes ]" :prepend t :empty-lines 1 :clock-keep t)
     ("e" "English" entry
      (id "929598fb-92c7-4321-9681-43e59a4f9d9f")
      "* NEW %?
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:
:LOGBOOK:
- Create time: %U
- From: %a
:END:" :prepend t :empty-lines 1 :clock-keep t)
     ("b" "Bookmark" entry
      (id "0822a2de-0d55-432c-967d-c2b2369df980")
      "* NEW %a
:LOGBOOK:
- Create time: %U
- From: %a
:END:
- URL: %L
- Tags: [add reference nodes ]
- Notes:" :prepend t :empty-lines 1 :clock-keep t)
     ("c" "Contacts" entry
      (id "f3c11ccd-31b0-45be-9046-42f6e6a2a7c6")
      "* %(org-contacts-template-name)
:PROPERTIES:
:COMPANY:
:POSITION:
:OCCUPATION:
:NOTE:
:PHONE:
:WeChat:
:WXWORK:
:EMAIL: %(org-contacts-template-email)
:ALITINGTING:
:QQ:
:ALIAS:
:NICKNAME:
:BIRTHDAY:
:ADDRESS:
:END:" :prepend t :empty-lines 1 :clock-keep t)
     ("x" "Password" entry
      (id "8c510a93-b780-4782-afbd-f61e38d42e25")
      "* %?
:LOGBOOK:
- Create time: %U
- From: %a
:END:
- Website:
- Username:
- Password:
- Tags: [add reference nodes ]
- Description:" :prepend t :empty-lines 1 :clock-keep t)))
 '(org-clock-history-length 10)
 '(org-clock-idle-time 15)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-switch-to-state "WAITING")
 '(org-clock-persist t)
 '(org-clock-persist-query-save t)
 '(org-clock-report-include-clocking-task t)
 '(org-clock-sound t)
 '(org-columns-default-format
   "%CATEGORY(Cat.) %PRIORITY(Pri.) %6TODO(State) %35ITEM(Details) %ALLTAGS(Tags) %5NUM_POMODORO(Plan){:} %6CLOCKSUM(Clock){Total} %SCORE(SCORE)")
 '(org-confirm-babel-evaluate nil)
 '(org-contacts-files '("~/org/roam/contacts.org.gpg"))
 '(org-crypt-disable-auto-save 'encrypt)
 '(org-crypt-key nil)
 '(org-default-notes-file "~/org/notes.org")
 '(org-directory "~/org")
 '(org-ditaa-eps-jar-path "/opt/DitaaEps/DitaaEps.jar")
 '(org-ditaa-jar-path "/opt/ditaa/ditaa.jar")
 '(org-download-edit-cmd "krita %s")
 '(org-download-image-org-width 200)
 '(org-download-method 'attach)
 '(org-download-screenshot-method "scrot -s %s")
 '(org-edit-src-turn-on-auto-save t)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii beamer html latex man md odt org texinfo))
 '(org-export-use-babel nil)
 '(org-export-with-sub-superscripts '{})
 '(org-format-latex-header
   "\\documentclass{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
% [removed] For displaying tikz pictures in latex fragments
% \\usepackage{tikz}
% \\usetikzlibrary{shadings}
% For displaying simplified chinese characters in latex fragments
\\usepackage{fontspec}
\\setmainfont{Noto Serif CJK SC}
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 2.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-global-properties
   '(("POMODORO_ALL" . "0 1 2 3 4 5")
     ("SCORE_ALL" . "0 1 2 3 4 5")))
 '(org-indirect-buffer-display 'current-window)
 '(org-latex-classes
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
 '(org-latex-compiler "xelatex")
 '(org-latex-listings 'minted)
 '(org-latex-minted-langs
   '((jupyter-python "python")
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
     (graphviz-dot "dot")))
 '(org-latex-minted-options
   '(("linenos" "true")
     ("mathescape" "")
     ("breaklines" "")
     ("fontsize" "\\footnotesize")
     ("frame" "lines")))
 '(org-latex-packages-alist '(("newfloat" "minted" nil)))
 '(org-latex-pdf-process
   '("latexmk -f -pdf -%latex -interaction=nonstopmode -shell-escape -output-directory=%o %f" "latexmk -c %f"))
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-log-redeadline 'note)
 '(org-log-refile 'time)
 '(org-log-reschedule 'time)
 '(org-log-state-notes-insert-after-drawers t)
 '(org-modules
   '(ol-bbdb ol-bibtex org-crypt ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe org-mouse org-protocol ol-rmail ol-w3m ol-elisp-symbol ol-git-link ol-man org-toc))
 '(org-plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\""))
 '(org-plantuml-jar-path "/opt/plantuml/plantuml.jar" t)
 '(org-preview-latex-default-process 'imagemagick)
 '(org-preview-latex-process-alist
   '((dvipng :programs
             ("latex" "dvipng")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
     (dvisvgm :programs
              ("xelatex" "dvisvgm")
              :description "xdv > svg" :message "you need to install the programs: xelatex and dvisvgm." :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("xelatex -shell-escape -no-pdf -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs
                  ("xelatex" "convert")
                  :description "pdf > png" :message "you need to install the programs: xelatex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 -colorspace RGB %O"))))
 '(org-refile-targets '((nil :maxlevel . 4) (org-agenda-files :maxlevel . 4)))
 '(org-refile-use-outline-path 'file)
 '(org-reverse-note-order t)
 '(org-roam-capture-ref-templates
   '(("a" "annote" entry "* NEW %?
:LOGBOOK:
- Create time: %U
:END:

${body}" :target
(file+head "${slug}.org" "#+title: ${title}
#+filetags: literature
# Time-stamp:  %U
- 📆Create time: %U
- ℹ️Create from: %a
- 🏷️Tags:  [ add reference notes ]
- 🧭Compass:
  + 🔼North:
  + ◀️West:
  + ▶️East:
  + 🔽South:

* 🚀Abstract

* 📚References
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:

[ If necessary, use org-web-tools-archive-attach to download a compressed copy. ]

- %a

* 📓Outcomes

[ Put reading notes here, which will be archived as permanent notes. ]")
:immediate-finish t :jump-to-captured t :empty-lines 1)))
 '(org-roam-capture-templates
   '(("d" "fleeting (default)" entry "* NEW %^{title}    :fleeting:
:LOGBOOK:
- Create time: %U
- From: %a
:END:
- 🏷️Tags:     [ add reference notes ]
- ℹ️See also: [ add fleeting and permanent notes ]" :target
(file+head "~/org/roam/note_inbox.org" "Notes")
:prepend t :empty-lines 1)
     ("l" "literature" entry "* NEW %?    :literature:
:LOGBOOK:
- Create time: %U
- From: %a
:END:" :target
(file+head "${slug}.org" "#+title: ${title}
#+filetags: literature
# Time-stamp:  %U
- 📆Create time: %U
- ℹ️Create from: %a
- 🏷️Tags:  [ add reference notes ]
- 🧭Compass:
  + 🔼North:
  + ◀️West:
  + ▶️East:
  + 🔽South:

* 🚀Abstract

* 📚References
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:

[ If necessary, use org-web-tools-archive-attach to download a compressed copy. ]

- %a

* 📓Outcomes

[ Put reading notes here, which will be archived as permanent notes. ]")
:empty-lines 1)
     ("p" "permanent" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+filetags: permanent
# Time-stamp:  %U
- 🚥Status: [[roam:INCOMING]]
- 🏷️Tags: [ add reference notes ]
- 🎛️Areas: [ add areas-of-responsibility ]

* 🧠Thought

* 📚References

* 🌏Context        :noexport:

** ❔Why noting

** 🧭Compass

- 🔼North: (Where X comes from, parent nodes)
- ◀️West: (What's similar to X, friend nodes)
- ▶️East: (What's opposite of X, friend nodes)
- 🔽South: (Where X leads to, child nodes)

** 💓Feelings

* 📜Change Logs        :noexport:

- %U Note was create from %a")
      :prepend t :empty-lines 1 :unnarrowed t)
     ("r" "reference" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+filetags: reference
# Time-stamp:  %U
- 📆Create time: %U
- ℹ️Create from: %a
- 🏷️Tags:  [ add reference notes ]
- 🧭Compass:
  + 🔼North:
  + ◀️West:
  + ▶️East:
  + 🔽South:

* About ${title}

[ basic information ]")
      :empty-lines 1 :unnarrowed t)
     ("h" "hub" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+filetags: hub
# Time-stamp:  %U
- Create time: %U
- From: %a")
      :empty-lines 1 :unnarrowed t nil nil)
     ("j" "project" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+category: ${title}
#+filetags: PROJECT
# Time-stamp:  %U
- Create time: %U
- Areas:    [ add area-of-responsibility tags ]
- Tags:     [ add reference notes ]
- See also: [ add fleeting and permanent notes ]

* Goal

[ Describe WHAT YOU REALLY WANT if the project was completed ]

* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:

** TODO Define the goal of the project.
** TODO Add areas-of-responsibility, tags, and exsisting notes related to this project.
** TODO Set a project deadline.
** Version 1.0
*** TODO Finish v1.0")
      :prepend t :clock-keep t :unnarrowed t)
     ("s" "software" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+filetags: reference
# Time-stamp:  %U
- Create time: %U
- Tags:  [[roam:software]], [ add reference notes ]
- See also: [ add fleeting and permanent notes ]

* About ${title}

[ Logo image ]
[ TL;DR - basic information about ${title} ]

* Resources

* Concepts and Terminology

* Install, Uninstall and Update

* Configuration

* Usage

** Basic usage

** Use cases

* Tricks & Tips")
      :prepend t :unnarrowed t)
     ("c" "code" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+filetags: code
#+PROPERTY: header-args:lang :tangle \"/path/to/tangled_source_code\" :mkdirp no
#+auto_tangle: t
# Time-stamp:  %U
- Create time: %U
- Origin: %a
- Tags:  [ add reference notes ]
- See also: [ add fleeting and permanent notes ]

* Description

* Code

* Tests

* References")
      :prepend t :empty-lines 1 :clock-keep t :unnarrowed t)))
 '(org-roam-completion-everywhere t)
 '(org-roam-dailies-capture-templates
   '(("d" "default" entry "** %U %?" :target
      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
* Mind Path

Track my mind of the day to help myself focus on the main tasks.

* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:")
      :empty-lines 1 :unnarrowed t)))
 '(org-roam-dailies-directory "~/org/dailies")
 '(org-roam-db-location "~/org/org-roam.db")
 '(org-roam-directory "~/org/roam")
 '(org-roam-list-files-commands '(rg))
 '(org-roam-mode-sections
   '(org-roam-backlinks-section org-roam-reflinks-section org-roam-unlinked-references-section))
 '(org-roam-protocol-store-links t)
 '(org-src-ask-before-returning-to-edit-buffer nil)
 '(org-src-preserve-indentation t)
 '(org-stuck-projects '("+PROJECT/-SOMEDAY-DONE" ("NEXT" "STARTED")))
 '(org-tag-persistent-alist
   '((:startgrouptag)
     ("PROJECT" . 80)
     ("AREA" . 65)
     ("RESOURCE" . 82)
     ("ARCHIVE" . 90)
     (:endgrouptag)
     (:startgrouptag)
     ("CONFIDENTIAL" . 67)
     ("FLAGGED" . 70)
     ("ATTACH" . 84)
     (:endgrouptag)
     (:startgrouptag)
     ("reference" . 114)
     ("literature" . 108)
     ("fleeting" . 102)
     ("permanent" . 112)
     ("code" . 99)
     ("hub" . 104)
     ("publication" . 98)
     (:endgrouptag)
     (:startgrouptag)
     ("noexport" . 78)
     ("TOC" . 84)
     (:endgrouptag)
     (:startgrouptag)
     ("action" . 116)
     ("hidden" . 120)
     ("status" . 115)
     (:endgrouptag)))
 '(org-todo-keywords
   '((sequence "TODO(t)" "SOMEDAY(x)" "NEXT(n)" "STARTED(s!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
     (sequence "NEW(a)" "REVIEW(r!)" "|" "MARK(m!)" "USELESS(u!)")))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-treat-insert-todo-heading-as-state-change t)
 '(org-use-property-inheritance "header-args\\|shebang\\|session\\|DIR\\|dir")
 '(org-use-tag-inheritance
   '("literature" "fleeting" "AREA" "RESOURCE" "ARCHIVE" "action" "status" "hidden"))
 '(org-wild-notifier-alert-time '(25 15 10 5 3 1))
 '(org-wild-notifier-keyword-blacklist '("DONE" "CANCELLED" "MARK" "USELESS"))
 '(org-wild-notifier-keyword-whitelist nil)
 '(org-wild-notifier-tags-blacklist '("ARCHIVE"))
 '(package-selected-packages
   '(flycheck git-commit alert powerline string-edit-at-point embark ess forge projectile markdown-mode macrostep transient request cdlatex auctex-latexmk aggressive-indent cargo company consult-org-roam consult dap-mode eaf ebib evil-collection evil-escape evil-nerd-commenter closql magit hl-todo js2-mode link-hint lsp-mode treemacs marginalia mmm-mode pdf-tools org-ref org-roam-bibtex bibtex-completion emacsql-sqlite emacsql parsebib org-tree-slide posframe quickrun dash inf-ruby helm helm-core unkillable-scratch web-mode async evil pcre2el use-package zoom-window youdao-dictionary yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode winum window-purpose which-key wgrep web-beautify volatile-highlights vim-powerline uuidgen unfill undo-tree typo-suggest treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-all-the-icons toml-mode toc-org terminal-here term-cursor tagedit systemd symon symbol-overlay subed string-edit sql-indent sphinx-doc spacemacs-whitespace-cleanup spacemacs-purpose-popwin spaceline-all-the-icons space-doc smeargle slime-company slim-mode shfmt shell-pop selectrum seeing-is-believing scss-mode sass-mode rvm rust-mode ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode ron-mode robe rime restart-emacs rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters pytest pylookup pyim-basedict pyim pyenv-mode pydoc py-isort pug-mode prettier-js popwin poetry plantuml-mode pippel pipenv pip-requirements pdf-view-restore password-generator paradox pangu-spacing ox-pandoc ox-gfm ox-epub overseer orgit-forge org-wild-notifier org-web-tools org-vcard org-transclusion org-superstar org-roam-ui org-rich-yank org-present org-pomodoro org-noter-pdftools org-mime org-fragtog org-fc org-download org-contrib org-contacts org-cliplink org-auto-tangle org-appear orderless open-junk-file ob-tmux ob-async npm-mode nov nose nodejs-repl nix-mode nameless mwim multi-vterm multi-term multi-line minitest markdown-toc magit-gitflow magic-latex-buffer lsp-ui lsp-treemacs lsp-python-ms lsp-pyright lsp-origami lsp-latex lsp-docker lorem-ipsum livid-mode live-py-mode ligature keycast jupyter json-reformat json-navigator json-mode js2-refactor js-doc journalctl-mode inspector insert-shebang info+ indent-guide importmagic impatient-mode ibuffer-projectile hybrid-mode hungry-delete holy-mode highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ hardhat graphviz-dot-mode goto-chg google-translate google-c-style golden-ratio gnuplot gitignore-templates git-timemachine git-modes git-messenger git-link git-gutter-fringe+ gif-screencast gh-md gendoxy fuzzy font-lock+ flyspell-popup flyspell-correct-popup flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-elsa flycheck-clj-kondo flycheck-bashate flx-ido fish-mode find-by-pinyin-dired fancy-narrow fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-tex evil-surround evil-org evil-numbers evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-evilified-state evil-ediff evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help engine-mode emr emojify emoji-cheat-sheet-plus emmet-mode embark-consult emamux elisp-slime-nav elisp-def editorconfig dumb-jump drag-stuff dotenv-mode dockerfile-mode docker disaster dired-quick-sort diminish devdocs demo-it define-word cython-mode csv-mode cpp-auto-include consult-yasnippet consult-lsp conda compleseus-spacemacs-help company-ycmd company-web company-statistics company-shell company-rtags company-reftex company-quickhelp company-plsense company-nixos-options company-math company-emoji company-c-headers company-auctex company-anaconda common-lisp-snippets command-log-mode column-enforce-mode color-identifiers-mode code-cells cmake-mode cmake-ide clojure-snippets clean-aindent-mode citeproc cider-eval-sexp-fu cider chruby chinese-conv cfrs centered-cursor-mode ccls cask bundler bui browse-at-remote blacken bind-key biblio aweshell auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile annalist all-the-icons-ibuffer all-the-icons-dired adoc-mode ace-window ace-pinyin ace-link ac-ispell))
 '(plantuml-indent-level 4)
 '(time-stamp-format " <%Y-%02m-%02d %3a %02H:%02M by %u on %s>")
 '(time-stamp-time-zone t)
 '(valign-fancy-bar t)
 '(writeroom-width 0.65))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t)
 '(keycast-command ((t (:inherit bold :extend t :foreground "#34cfff" :underline t :height 1.3 :family "monospace"))))
 '(keycast-key ((t (:inherit bold :background "#34cfff" :foreground "black" :box (:line-width (2 . 2) :color "spring green" :style pressed-button) :height 1.6 :family "monospace")))))
