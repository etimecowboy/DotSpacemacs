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
   "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \12\12See [[cite:&${=key=}]]\12")
 '(bibtex-completion-pdf-open-function
   '(closure
        (t)
        (fpath)
      (call-process "open" nil 0 nil fpath)))
 '(blink-cursor-interval 0.3)
 '(blink-cursor-mode t)
 '(company-emoji-insert-unicode t t)
 '(connection-local-criteria-alist
   '(((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "dad40020beea412623b04507a4c185079bff4dcea20a93d8f8451acb6afc8358" "a0415d8fc6aeec455376f0cbcc1bee5f8c408295d1c2b9a1336db6947b89dd98" "d600c677f1777c1e4bfb066529b5b73c0179d0499dd4ffa3f599a0fb0cfbd501" default))
 '(ispell-program-name "hunspell")
 '(lsp-keymap-prefix "M-#")
 '(marginalia-separator "  |  ")
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
 '(org-attach-use-inheritance t)
 '(org-capture-templates
   '(("t" "Task" entry
      (id "c99c005d-0aaa-46dd-b889-f8579726aa2a")
      "* TODO %?\12:LOGBOOK:\12- Create time: %U\12- From: %a\12:END:\12- Tags:     [ add exsisting reference notes as tags]\12- See also: [ add existing literate, fleeting, and permanent notes that relates with this project ]" :prepend t :empty-lines 1 :clock-keep t)
     ("n" "Note" entry
      (id "eb39c457-7821-4600-85a8-e8fa76d328ab")
      "* NEW %?    :fleeting:\12:LOGBOOK:\12- Create time: %U\12- From: %a\12:END:\12- Tags:     [ add reference notes ]\12- See also: [ add fleeting and permanent notes ]" :prepend t :empty-lines 1 :clock-keep t)
     ("e" "English" entry
      (id "929598fb-92c7-4321-9681-43e59a4f9d9f")
      "* NEW %?\12:PROPERTIES:\12:ROAM_EXCLUDE: t\12:END:\12:LOGBOOK:\12- Create time: %U\12- From: %a\12:END:" :prepend t :empty-lines 1 :clock-keep t)
     ("b" "Bookmark" entry
      (id "0822a2de-0d55-432c-967d-c2b2369df980")
      "* NEW %a\12:LOGBOOK:\12- Create time: %U\12- From: %a\12:END:\12- URL: %L\12- Tags: [add reference nodes ]\12- Notes:" :prepend t :empty-lines 1 :clock-keep t)
     ("c" "Contacts" entry
      (id "f3c11ccd-31b0-45be-9046-42f6e6a2a7c6")
      "* %(org-contacts-template-name)\12:PROPERTIES:\12:COMPANY:\12:POSITION:\12:OCCUPATION:\12:NOTE:\12:PHONE:\12:WeChat:\12:WXWORK:\12:EMAIL: %(org-contacts-template-email)\12:ALITINGTING:\12:QQ:\12:ALIAS:\12:NICKNAME:\12:BIRTHDAY:\12:ADDRESS:\12:END:" :prepend t :empty-lines 1 :clock-keep t)
     ("x" "Password" entry
      (id "8c510a93-b780-4782-afbd-f61e38d42e25")
      "* %?\12:LOGBOOK:\12- Create time: %U\12- From: %a\12:END:\12- Website:\12- Username:\12- Password:\12- Tags: [add reference nodes ]\12- Description:" :prepend t :empty-lines 1 :clock-keep t)))
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
   "\\documentclass{article}\12\\usepackage[usenames]{color}\12[PACKAGES]\12[DEFAULT-PACKAGES]\12% [removed] For displaying tikz pictures in latex fragments\12% \\usepackage{tikz}\12% \\usetikzlibrary{shadings}\12% For displaying simplified chinese characters in latex fragments\12\\usepackage{fontspec}\12\\setmainfont{Noto Serif CJK SC}\12\\pagestyle{empty}             % do not remove\12% The settings below are copied from fullpage.sty\12\\setlength{\\textwidth}{\\paperwidth}\12\\addtolength{\\textwidth}{-3cm}\12\\setlength{\\oddsidemargin}{1.5cm}\12\\addtolength{\\oddsidemargin}{-2.54cm}\12\\setlength{\\evensidemargin}{\\oddsidemargin}\12\\setlength{\\textheight}{\\paperheight}\12\\addtolength{\\textheight}{-\\headheight}\12\\addtolength{\\textheight}{-\\headsep}\12\\addtolength{\\textheight}{-\\footskip}\12\\addtolength{\\textheight}{-3cm}\12\\setlength{\\topmargin}{1.5cm}\12\\addtolength{\\topmargin}{-2.54cm}")
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
 '(org-latex-src-block-backend 'minted)
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
 '(org-plantuml-args '("-headless" "-DRELATIVE_INCLUDE=\".\""))
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
 '(org-refile-use-outline-path 'title)
 '(org-reverse-note-order t)
 '(org-roam-capture-ref-templates
   '(("a" "annote" plain "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\12#+filetags: reference PROJECT\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨Create from: %a\12- 🎛️Areas: [ add areas-of-responsibility ]\12- 🏷️Tags:  [ add reference notes ]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* 🚀Abstract\12\12* 📚References\12:PROPERTIES:\12:ROAM_EXCLUDE: t\12:END:\12\12[ If necessary, use org-web-tools-archive-attach to download a compressed copy. ]\12\12- %a\12\12* 📔Outcomes\12\12[ Put reading notes here, which will be archived as permanent notes. ]\12\12* 🔧Tasks\12\12** TODO Quickly scan useful contents of %a")
      :immediate-finish t :jump-to-captured t :empty-lines 1)))
 '(org-roam-capture-templates
   '(("d" "fleeting (default)" entry "* NEW %^{title}    :fleeting:\12:LOGBOOK:\12- Create time: %U\12- From: %a\12- Tags: [ add reference notes ]\12- See also: [ add fleeting and permanent notes ]\12- Areas: [ add areas-of-responsibility ]\12:END:" :target
      (file "~/org/roam/note_inbox.org")
      :prepend t :empty-lines 1)
     ("l" "literature" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\12#+filetags: literature\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨️Create from: %a\12- 🎛️Areas: [ add areas-of-responsibility if applicable]\12- 🏷️Tags:  [ add reference notes ]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* 🧠Thought\12\12* 📚References")
      :empty-lines 1)
     ("p" "permanent" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\12#+filetags: permanent\12# Time-stamp:  %U\12- 🚥Status: [[roam:INCOMING]]\12- 🏷️Tags: [ add reference notes ]\12- 🎛️Areas: [ add areas-of-responsibility ]\12\12* 🧠Thought\12\12* 📚References\12\12* 🌏Context        :noexport:\12\12** ❔Why noting\12\12** 🧭Compass\12\12- 🔼North: (Where X comes from, parent nodes)\12- ◀️West: (What's similar to X, friend nodes)\12- ▶️East: (What's opposite of X, friend nodes)\12- 🔽South: (Where X leads to, child nodes)\12\12** 💓Feelings\12\12* 📜Change Logs        :noexport:\12\12- %U Note was create from %a")
      :prepend t :empty-lines 1 :unnarrowed t)
     ("r" "reference" plain "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\12#+filetags: reference\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨️Create from: %a\12- 🎛️Areas: [ add areas-of-responsibility ]\12- 🏷️Tags:  [ add reference notes ]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* 🚀Abstract\12\12* 📚References\12:PROPERTIES:\12:ROAM_EXCLUDE: t\12:END:\12\12[ If necessary, use org-web-tools-archive-attach to download a compressed copy. ]\12\12- %a\12\12* 📓Outcomes\12\12[ Put reading notes here, which will be archived as permanent notes. ]")
      :jump-to-captured t :empty-lines 1)
     ("g" "glossary" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\12#+filetags: glossary\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨Create from: %a\12- 🏷️Tags:  [ add reference notes ]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* ℹ️About ${title}\12\12[ basic information ]\12\12* [[roam:${title} concepts and terminologies]]\12\12* 📚References")
      :empty-lines 1 :unnarrowed t)
     ("h" "hub" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\12#+filetags: hub\12# Time-stamp:  %U\12- Create time: %U\12- From: %a")
      :empty-lines 1 :unnarrowed t nil nil)
     ("j" "project" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\12#+category: ${title}\12#+filetags: PROJECT\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨️Create from: %a\12- 🎛️Areas: [ add areas-of-responsibility ]\12- 🏷️Tags:  [ add reference notes ]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* 🥅Goal\12\12[ Describe WHAT YOU REALLY WANT if the project was completed ]\12\12* 🔧Tasks\12:PROPERTIES:\12:ROAM_EXCLUDE: t\12:END:\12\12** TODO Define the goal of the project.\12** TODO Add areas-of-responsibility, tags, and exsisting notes related to this project.\12** TODO Set a project deadline.\12** Version 1.0\12*** TODO Finish v1.0")
      :prepend t :clock-keep t :unnarrowed t)
     ("v" "vocabulary" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\12#+filetags: :vocabulary:fleeting:\12# Time-stamp:  %U\12- Create time: %U\12- From: %a\12- Tags: [[roam:English Language Inbox]]")
      :empty-lines 1 :unnarrowed t nil nil)
     ("s" "software" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\12#+filetags: glossary\12# Time-stamp:  %U\12- 📆Create time: %U\12- ✨Create from: %a\12- 🏷️Tags:  [[roam:software]]\12- 🧭Compass:\12  + 🔼North:\12  + ◀️West:\12  + ▶️East:\12  + 🔽South:\12\12* ℹ️About ${title}\12\12[ Logo image ]\12[ TL;DR - basic information about ${title} ]\12\12* [[roam:${title} concepts and terminologies]]\12\12* [[roam:${title} maintenance work]]\12** [[roam:Install ${title} on Ubuntu]]\12** [[roam:${title} configuration]]\12** [[roam:${title} extensions and plugins]]\12\12* [[roam:${title} useful resources]]\12\12* [[roam:${title} tips and tricks]]")
      :unnarrowed t)
     ("c" "code" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\12#+filetags: code\12#+PROPERTY: header-args:lang :tangle \"/path/to/tangled_source_code\" :mkdirp no\12#+auto_tangle: t\12# Time-stamp:  %U\12- Create time: %U\12- Origin: %a\12- Tags:  [ add reference notes ]\12- See also: [ add fleeting and permanent notes ]\12\12* Description\12\12* Code\12\12* Tests\12\12* References")
      :prepend t :empty-lines 1 :clock-keep t :unnarrowed t)))
 '(org-roam-completion-everywhere t)
 '(org-roam-dailies-capture-templates
   '(("d" "default" entry "** %U %?" :target
      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\12* Mind Path\12\12Track my mind of the day to help myself focus on the main tasks.\12\12* Tasks\12:PROPERTIES:\12:ROAM_EXCLUDE: t\12:END:")
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
     ("glossary" . 103)
     ("reference" . 114)
     ("literature" . 108)
     ("fleeting" . 102)
     ("permanent" . 112)
     ("code" . 99)
     ("hub" . 104)
     ("publication" . 98)
     (:endgrouptag)
     (:startgrouptag)
     ("noexport" . 110)
     ("TOC" . 79)
     ("repeat" . 114)
     (:endgrouptag)
     (:startgrouptag)
     ("action" . 116)
     ("hidden" . 104)
     ("status" . 115)
     (:endgrouptag)
     ("vocabulary" . 118)))
 '(org-todo-keywords
   '((sequence "TODO(t)" "SOMEDAY(x)" "NEXT(n)" "STARTED(s!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
     (sequence "NEW(a)" "REVIEW(r!)" "|" "MARK(m!)" "USELESS(u!)")))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-treat-insert-todo-heading-as-state-change t)
 '(org-use-property-inheritance "header-args\\|shebang\\|session\\|DIR\\|dir")
 '(org-use-tag-inheritance
   '("fleeting" "AREA" "RESOURCE" "ARCHIVE" "action" "status" "hidden" "publication" "code" "vocabulary" "ATTACH"))
 '(org-wild-notifier-alert-time '(25 15 10 5 3 1))
 '(org-wild-notifier-keyword-blacklist '("DONE" "CANCELLED" "MARK" "USELESS"))
 '(org-wild-notifier-keyword-whitelist nil)
 '(org-wild-notifier-tags-blacklist '("ARCHIVE"))
 '(package-selected-packages
   '(acm-terminal popon consult embark haml-mode pdf-tools tablist treemacs posframe async djvu avy all-the-icons eaf magit projectile inspector lsp-bridge macrostep marginalia nix-mode transient pyim helm helm-core web-mode evil ws-butler flycheck-rust pyenv-mode emmet-mode aggressive-indent restart-emacs lorem-ipsum conda multi-line rubocopfmt spacemacs-purpose-popwin spacemacs-whitespace-cleanup company-plsense fish-mode company-c-headers esh-help subed spaceline-all-the-icons column-enforce-mode pangu-spacing hide-comnt command-log-mode toml-mode compleseus-spacemacs-help tagedit flycheck-rtags company-web emoji-cheat-sheet-plus ivy-bibtex undo-tree fuzzy flycheck-bashate company-shell git-gutter-fringe hyperbole rime google-translate symon org-pomodoro insert-shebang pip-requirements keycast flyspell-popup company-math vim-powerline journalctl-mode json-mode indent-guide quickrun flycheck-pos-tip emacs-everywhere eval-sexp-fu company-statistics graphviz-dot-mode livid-mode aweshell magic-latex-buffer rvm consult-projectile poetry rubocop pyim-basedict elisp-slime-nav zoom-window evil-lisp-state web-beautify sql-indent org-fc link-hint mmm-mode markdown-toc ruby-refactor paradox info+ seeing-is-believing highlight-numbers systemd help-fns+ org-rich-yank popwin disaster watch-other-window uuidgen eyebrowse emamux ibuffer-projectile diminish company-quickhelp chinese-conv pydoc org-auto-tangle symbol-overlay ivy-yasnippet eshell-z rake json-reformat tree-sitter-langs engine-mode cython-mode auto-dictionary font-lock+ rainbow-delimiters holy-mode google-c-style drag-stuff org-appear consult-yasnippet company-reftex nov sphinx-doc gitignore-templates smeargle org-ref typo-suggest dired-quick-sort expand-region cargo evil-cleverparens ruby-test-mode highlight-parentheses xr devdocs npm-mode flx-ido git-link evil-evilified-state git-modes unkillable-scratch scss-mode org-transclusion ivy-purpose org-fragtog org-contrib py-isort yapfify define-word multi-vterm browse-at-remote nameless hybrid-mode highlight-indentation rspec-mode org-superstar nose rbenv ace-link nodejs-repl robe eshell-prompt-extras consult-dir js2-refactor minitest toc-org bundler fancy-battery org-roam-bibtex auto-compile centered-cursor-mode wgrep shell-pop consult-company company-ycmd dumb-jump git-commit company-box org-web-tools clean-aindent-mode flyspell-correct-popup blacken org-mime org-vcard ebib ron-mode org-present treemacs-icons-dired cpp-auto-include demo-it pdf-view-restore company-nixos-options org-wild-notifier pylookup ruby-tools find-by-pinyin-dired goto-chg xterm-color jupyter auto-highlight-symbol treemacs-persp csv-mode sqlup-mode cmake-ide company-rtags gif-screencast treemacs-magit company-tabnine all-the-icons-ibuffer password-generator chruby ob-tmux golden-ratio org-roam-ui org-noter-pdftools hl-todo impatient-mode gendoxy orderless importmagic unfill org-download magit-gitflow fancy-narrow dotenv-mode mwim slim-mode org-tree-slide auto-yasnippet multi-term shfmt company-auctex org-cliplink editorconfig youdao-dictionary racer which-key open-junk-file winum gh-md cmake-mode org-contacts ace-pinyin hungry-delete elisp-def git-messenger js-doc yasnippet-snippets cdlatex consult-org-roam counsel-css dockerfile-mode space-doc live-py-mode counsel-projectile sass-mode pug-mode ruby-hash-syntax orgit-forge flycheck-ycmd company-emoji cliphist ivy-rtags prettier-js flycheck-package popweb vertico pytest embark-consult all-the-icons-dired yaml-mode emr overseer volatile-highlights plantuml-mode emojify code-cells json-navigator flycheck-elsa pippel string-edit-at-point term-cursor docker gnuplot company-anaconda adoc-mode ox-hugo terminal-here treemacs-projectile ox-epub ac-ispell pipenv git-timemachine writeroom-mode))
 '(plantuml-indent-level 4)
 '(safe-local-variable-values
   '((conda-project-env-path . "/home/xin/.conda/envs/gallery-dl")
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp)))
 '(time-stamp-format " <%Y-%02m-%02d %3a %02H:%02M by %u on %s>")
 '(time-stamp-time-zone t)
 '(undo-tree-auto-save-history nil)
 '(valign-fancy-bar t)
 '(writeroom-width 0.65)
 '(xref-search-program 'ripgrep))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "lime green"))))
 '(highlight-indentation-current-column-face ((t (:background "gold"))))
 '(highlight-indentation-face ((t (:background "dim gray"))))
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t)
 '(keycast-command ((t (:inherit bold :extend t :foreground "#34cfff" :underline t :height 1.3 :family "monospace"))))
 '(keycast-key ((t (:inherit bold :background "#34cfff" :foreground "black" :box (:line-width (2 . 2) :color "spring green" :style pressed-button) :height 1.6 :family "monospace")))))
