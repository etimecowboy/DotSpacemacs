;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- org-extra layer packages file for Spacemacs.
;; Time-stamp: <2024-01-24 Wed 07:45 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst org-extra-packages
  '(
    ;;----- org layer packages
    org
    org-contacts
    org-download
    org-ref
    markdown
    org-roam
    org-roam-ui
    alert
    org-wild-notifier
    org-modern
    ;;----- added packages
    djvu
    org-noter
    org-pdftools
    org-noter-pdftools
    org-fragtog
    (org-roam-bibtex :requires org-roam)
    org-fc
    ;; (org-fc :location (recipe :fetcher git :url "https://git.sr.ht/~l3kn/org-fc"
    ;;                           :files (:defaults "awk" "demo.org")))
    org-web-tools
    org-auto-tangle
    (ob-async :location local) ;; NOTE: Use patched version.
    ;;----- abandoned packages
    ;; ob-ipython ;; replaced by jupyter
    ;; ob-restclient ;; owned in restclient layer
    ;; org-inline-anim
    ;; org-special-block-extras
    ;; maxpix
    ;; equation to latex code generation, requires a paid appkey
    ;; - https://github.com/jethrokuan/mathpix.el
    ;; - https://accounts.mathpix.com/account
    ;; (maxthpix
    ;;  :location (recipe :fetcher github :repo "jethrokuan/mathpix.el"))
    ;; org-tanglesync ;; not very useful
    ;; org-appear
    ))

(defun org-extra/pre-init-org ()

  (spacemacs|use-package-add-hook org
    ;; (spacemacs/add-to-hook 'org-mode-hook '(xy/adapt-org-config))
    :pre-init
    (setq org-directory "~/org/"
          org-default-notes-file "~/org/notes.org")

    (setq org-modules '(ol-bbdb ol-bibtex org-crypt org-ctags ol-docview ol-doi ol-eww
                                ol-gnus org-id ol-info org-inlinetask ol-irc org-habit
                                ol-mhe org-mouse org-protocol ol-rmail ol-w3m ol-eshell
                                ol-bookmark ol-elisp-symbol org-eval ol-man org-toc
                                ))

    :post-init
    (add-hook 'after-save-hook #'org-redisplay-inline-images)
    ;; (add-hook 'org-mode-hook #'toc-org-mode)
    (add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list)
    ;; a crazy nyan cat!!!
    ;; (if (featurep 'nyan-mode)
    ;;     (progn
    ;;       (spacemacs/add-to-hook 'org-agenda-finalize-hook '(nyan-start-music))
    ;;       (advice-add 'org-agenda-quit :before 'nyan-stop-music)))
    ;; (when (featurep 'emms)
    ;;   (spacemacs/add-to-hook 'org-agenda-finalize-hook
    ;;                          '((lambda ()
    ;;                              (emms-play-playlist "~/音乐/bilibili-summer_study_room"))))
    ;;   ;;(advice-add 'org-agenda-quit :before 'emms-pause)
    ;;   )

    :post-config
    (setq org-directory "~/org/"
          org-default-notes-file "~/org/notes.org")

    ;; org fast keys
    ;; https://www.youtube.com/watch?v=v-jLg1VaYzo

    (setq org-use-speed-commands
          (lambda () (and (looking-at org-outline-regexp)
                          (looking-back "^\**"))))
    ;; (setq org-speed-commands (cons '("w" . widen) org-speed-commands))

    ;; NOTE: dynamically set `org-indirect-buffer-display' in
    ;; `xy/org-adapt-config'
    ;;
    ;; (setq org-indirect-buffer-display 'other-window)
    ;;
    ;; NOTE: Set in `demo' layer, `fancy-narrow' package
    ;; (setq org-speed-commands
    ;;       (cons '("S" . org-fancy-narrow-to-subtree) org-speed-commands))

    (setq org-startup-indented t)

    ;; REF: https://emacs.stackexchange.com/questions/70477/how-do-i-insert-pipes-at-the-beginning-of-a-line-in-orgmode-without-them-being-c
    (add-to-list 'org-entities-user
                 '("zwsp"
                   "\\hspace{0pt}" ; latex
                   nil             ; not in math-mode
                   "&#8203;"       ; html
                   ""              ; ascii
                   nil             ; latin1 not sure what to put here
                   "​"              ; utf-8
                   ))

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
          "\\documentclass{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
% [removed] For displaying tikz pictures in latex fragments
% \\usepackage{tikz}
% \\usetikzlibrary{shadings}
% For displaying simplified chinese characters in latex fragments
\\usepackage{bm}
\\usepackage[os=win]{menukeys}
\\renewmenumacro{\\keys}[+]{roundedkeys}
\\renewmenumacro{\\menu}[>]{roundedmenus}
\\renewmenumacro{\\directory}[/]{hyphenatepathswithblackfolder}
\\usepackage{fontspec}
\\usepackage{fontawesome}
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
\\addtolength{\\topmargin}{-2.54cm}
\\DeclareMathOperator*{\\argmax}{argmax}\\DeclareMathOperator*{\\argmin}{argmin}")

    (setq org-format-latex-options
          '(:foreground default
            :background default
            ;; :foreground "black"
            ;; :background "white"
            :scale 2
            :html-foreground "Black"
            :html-background "Transparent"
            :html-scale 2
            :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

    ;; NOTE: LaTeX header that will be used when processing a fragment
    ;;   (setq org-format-latex-header
    ;;         "\\documentclass{article}
    ;; \\usepackage[usenames]{color}
    ;; [PACKAGES]
    ;; [DEFAULT-PACKAGES]
    ;; \\usepackage{tikz}
    ;; \\usetikzlibrary{
    ;; arrows, calc, fit, patterns, plotmarks, shapes, shadows,
    ;; datavisualization, er, automata, backgrounds, chains, topaths,
    ;; trees, matrix, fadings, shadings, through, positioning, scopes,
    ;; intersections, fixedpointarithmetic, petri,
    ;; decorations.pathreplacing, decorations.pathmorphing,
    ;; decorations.markings}
    ;; \\usepackage{pgfgantt}

    ;; \\pagestyle{empty}             % do not remove
    ;; % The settings below are copied from fullpage.sty
    ;; \\setlength{\\textwidth}{\\paperwidth}
    ;; \\addtolength{\\textwidth}{-3cm}
    ;; \\setlength{\\oddsidemargin}{1.5cm}
    ;; \\addtolength{\\oddsidemargin}{-2.54cm}
    ;; \\setlength{\\evensidemargin}{\\oddsidemargin}
    ;; \\setlength{\\textheight}{\\paperheight}
    ;; \\addtolength{\\textheight}{-\\headheight}
    ;; \\addtolength{\\textheight}{-\\headsep}
    ;; \\addtolength{\\textheight}{-\\footskip}
    ;; \\addtolength{\\textheight}{-3cm}
    ;; \\setlength{\\topmargin}{1.5cm}
    ;; \\addtolength{\\topmargin}{-2.54cm}")

    ;;   (setq org-format-latex-options
    ;;         '(:foreground default
    ;;                       :background default
    ;;                       :scale 1.0
    ;;                       :html-foreground "Black"
    ;;                       :html-background "Transparent"
    ;;                       :html-scale 1.0
    ;;                       :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

    (setq org-format-latex-signal-error t)
    (setq org-latex-create-formula-image-program 'imagemagick)

    ;; Use latexmk instead of xelatex
    ;; (setq org-latex-pdf-process
    ;;       '("latexmk -pdf -bibtex -f -silent %b"
    ;;         "latexmk -c"))

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
                                   (shell-script "tmux")
                                   (shell-script "shell")
                                   (shell-script "sh")
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
             :image-converter ("convert -density %D -trim -antialias %f -quality 100 -colorspace RGB %O"))))

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

    (add-list-to-list 'org-src-lang-modes
                      '(("latex" . latex )
                        ("emacs-lisp" . emacs-lisp )
                        ("python" . python)
                        ("jupyter" . python)
                        ))

    ;; Set source block faces
    ;; REF: https://stackoverflow.com/questions/44811679/orgmode-change-code-block-background-color
    (setq org-src-block-faces
          '(;; compiled languages
            ("C" (:background "unspecified-bg"))
            ("C++" (:background "unspecified-bg"))
            ("go" (:background "unspecified-bg"))
            ("rust" (:background "unspecified-bg"))
            ;; script languages
            ("emacs-lisp" (:background "unspecified-bg"))
            ("elisp" (:background "unspecified-bg"))
            ("python" (:background "unspecified-bg"))
            ("perl" (:background "unspecified-bg"))
            ("ruby" (:background "unspecified-bg"))
            ("matlab" (:background "unspecified-bg"))
            ("r" (:background "unspecified-bg"))
            ;; shell languages
            ("shell" (:background "navy"))
            ("bash" (:background "navy"))
            ("sh" (:background "navy"))
            ("tmux" (:background "navy"))
            ;; config languages
            ("conf" (:background "SlateGray4"))
            ("yaml" (:background "SlateGray4"))
            ("json" (:background "SlateGray4"))
            ("lua" (:background "SlateGray4"))
            ;; markup languages
            ("org" (:background "gray30"))
            ("latex" (:background "gray30"))
            ("markdown" (:background "gray30"))
            ;; graphic description languages
            ("plantuml" (:background "unspecified-bg"))
            ("graphviz" (:background "unspecified-bg"))
            ("dot" (:background "unspecified-bg"))
            ))

    (setq org-stuck-projects '("+PROJECT/-SOMEDAY-DONE" ("NEXT" "STARTED")))

    (setq org-tag-persistent-alist
          '(
            ;; states of writing
            (:startgroup) ("INCOMING" . ?0) ("DRAFTING" . ?1) ("FINISHED" . ?2)
            ("STACKED" . ?3) ("REVISING" . ?4) ("PUBLISHED" . ?5)
            ("REDIRECTED" . ?9)  (:endgroup)

            ;; My processing result
            (:startgroup) ("UNCERTAIN" . ?u) ("VERIFIED" . ?V) ("FAILED" . ?X)
            ("ADOPTED" . ?O) ("DEPRECATED" . ?d) (:endgroup)

            ;; My personal attitude
            (:startgrouptag) ("MYOWN" . ?M) ("PREFERRED" . ?L) ("DIS" . ?D)
            (:endgrouptag)

            ;; My decision on note value, it is easy to insert to tag, no need
            ;; for fast keys
            ;; (:startgroup) ("A" . ?1) ("B" . ?2) ("C" . ?3) ("D" . ?4) ("E" . ?5)
            ;; (:endgroup)

            ;; Zettelkasten method
            (:startgroup) ("glossary" . ?g) ("reference" . ?r)
            ("literature" . ?l) ("fleeting" . ?f) ("permanent" . ?p)
            ("hub" . ?h) (:endgroup)

            ;; types of resources
            (:startgrouptag) ("code" . ?c) ("config" . ?o) ("data" . ?d)
            ("tip" . ?t) ("example" . ?e) ("vocabulary" . ?v)
            ("quotation" . ?q) (:endgrouptag)

            ;; categories defined by fast reading (meta learning, for literature
            ;; and permanent notes)
            (:startgroup) ("concept" . ?x) ("fact" . ?a) ("procedure" . ?m)
            (:endgroup)

            ;; PARA method
            (:startgroup) ("PROJECT" . ?P) ("AREA" . ?A) ("RESOURCE" . ?R)
            ("ARCHIVE" . ?Z) (:endgroup)

            ;; tags used by org and org extensions
            (:startgrouptag) ("FLAGGED" . ?F) ("ATTACH" . ?H)
            ("crypt" . ?Y) ("noexport" . ?N) ("TOC" . ?i) ("fc" . ?~)
            (:endgrouptag)

            ;; GTD state
            (:startgrouptag) ("repeat" . ?*) ("CONFIDENTIAL" . ?C)
            (:endgrouptag)

            ;; (:startgrouptag) ("action" . ?a) ("hidden" . ?i)
            ;; ("status" . ?s) (:endgrouptag)
            ))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "SOMEDAY(x)" "NEXT(n)"
                      "STARTED(s!)" "WAITING(w!)" "|"
                      "DONE(d!)" "CANCELLED(c@/!)")
            (sequence "NEW(a)" "REVIEW(r!)" "|"
                      "MARK(m!)" "USELESS(u!)")))

    (setq org-treat-S-cursor-todo-selection-as-state-change nil
          org-treat-insert-todo-heading-as-state-change t)

    (setq org-use-property-inheritance "header-args\\|shebang\\|session\\|DIR\\|dir"
          org-use-tag-inheritance '("PROJECT" "AREA" "RESOURCE" "ARCHIVE" "ATTACH"))
                                    ;; "action" "status" "hidden" "publication"
                                    ;; "code" "vocabulary" "quotation"

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
              ;; NOTE: I would like to have a general `REVIEW' state instead of
              ;; fc specific flashcard.
              ;;
              ;; (if
              ;;     (string= org-state "REVIEW")
              ;;     (org-fc-type-vocab-init))
              (if
                  (string= org-state "MARK")
                  (progn
                    (org-set-tags ":permanent:")
                    (org-roam-extract-subtree)))
              )))

    (setq org-capture-templates
          '(("t" "Task" entry
             (file "~/org/roam/task_inbox.org")
             (file "templates/task.org")
             :prepend t :empty-lines 1 :clock-keep t)
            ("n" "Note" entry
             (file "~/org/roam/note_inbox.org")
             (file "templates/fleeting.org")
             :prepend t :empty-lines 1 :clock-keep t :jump-to-captured t)
            ("v" "Vocabulary" entry
             (file "~/org/roam/english_language_inbox.org")
             (file "templates/vocab.org")
             :prepend t :empty-lines 1 :clock-keep t)
            ("b" "Bookmark" entry
             (file "~/org/roam/bookmark_inbox.org")
             (file "templates/bookmark.org")
             :prepend t :empty-lines 1 :clock-keep t :immediate-finish t)
            ("c" "Contacts" entry
             (file "~/org/roam/contacts.org.gpg")
             (file "templates/contact.org")
             :prepend t :empty-lines 1 :clock-keep t)
            ("x" "Password" entry
             (file "~/org/roam/passwords.org.gpg")
             (file "templates/password.org")
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
          org-clock-out-switch-to-state "STARTED"
          org-clock-persist t
          org-clock-persist-query-save t
          org-clock-report-include-clocking-task t
          org-clock-sound t)

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
          org-agenda-window-setup 'only-window
          org-agenda-time-grid '((daily today require-timed)
                                 (800 1000 1200 1400 1600 1800 2000)
                                 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
          org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────")

    (setq org-babel-load-languages '((awk . t) (C . t) (calc . t) (css . t)
                                     (ditaa . t) (dot . t)
                                     (emacs-lisp . t) (eshell . t) (lisp . t)
                                     (latex . t) (lua . t) (makefile . t)
                                     (matlab .t ) (octave . t)
                                     (org . t) (perl . t) (plantuml . t)
                                     (python . t) (ruby . t) (sed . t) (shell . t)
                                     (sql . t) (sqlite . t)
                                     ))

    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

    ;; (require 'ob)
    ;; (require 'ob-awk)
    ;; (require 'ob-C)
    ;; (require 'ob-calc)
    ;; (require 'ob-css)
    ;; (require 'ob-ditaa)
    ;; (require 'ob-dot)
    ;; (require 'ob-emacs-lisp)
    ;; (require 'ob-eshell)
    ;; (require 'ob-latex)
    ;; (require 'ob-lisp)
    ;; (require 'ob-lua)
    ;; (require 'ob-makefile)
    ;; (require 'ob-matlab)
    ;; (require 'ob-octave)
    ;; (require 'ob-org)
    ;; (require 'ob-perl)
    ;; (require 'ob-plantuml)
    ;; (require 'ob-python)
    ;; (require 'ob-sed)
    ;; (require 'ob-shell)
    ;; (require 'ob-sql)
    ;; (require 'ob-sqlite)

    ;; Expend babel variables in exported org
    ;;
    ;; REF: https://emacs.stackexchange.com/questions/49092/passing-variable-into-a-org-babel-code-on-export
    ;;
    ;; NOTE: there is a `ob-org.el'
    ;;
    ;; (defun org-babel-execute:org (body params)
    ;;   "Return BODY with variables from PARAMS replaced by their values."
    ;;   (let* ((vars (cl-loop for par in params
    ;;                         if (eq (car par) :var)
    ;;                         collect (cons (symbol-name (cadr par)) (cddr par))))
    ;;          (re (regexp-opt (mapcar #'car vars) 'words))
    ;;          (pos 0))
    ;;     (while (string-match re body pos)
    ;;       (setq body (replace-match
    ;;                   (format "%s"
    ;;                           (cdr (assoc-string (match-string 0 body) vars)))
    ;;                   nil nil
    ;;                   body)))
    ;;     body))

    ;; Expend babel variables in exported conf
    (defun org-babel-execute:conf (body params)
      "Return BODY with variables from PARAMS replaced by their values."
      (let* ((vars (cl-loop for par in params
                            if (eq (car par) :var)
                            collect (cons (symbol-name (cadr par)) (cddr par))))
             (re (regexp-opt (mapcar #'car vars) 'words))
             (pos 0))
        (while (string-match re body pos)
          (setq body (replace-match
                      (format "%s"
                              (cdr (assoc-string (match-string 0 body) vars)))
                      nil nil
                      body)))
        body))

    ;; -- Fix inline image display problem -----------------------------------------

    (require 'subr-x)

    (defun xy/org-babel-after-execute ()
      "Redisplay inline images after executing source blocks with graphics results."
      (when-let ((info (org-babel-get-src-block-info t))
                 (params (org-babel-process-params (nth 2 info)))
                 (result-params (cdr (assq :result-params params)))
                 ((member "graphics" result-params)))
        (org-redisplay-inline-images)))

    (ad-activate 'org-babel-execute-src-block)
    (add-hook 'org-babel-after-execute-hook #'xy/org-babel-after-execute)

    ;; (add-hook 'org-babel-after-execute-hook #'xy/org-babel-after-execute)
    ;; (add-hook 'before-save-hook #'org-redisplay-inline-images)

    ;; -- Reference function -----------------

    ;; (defun shk-fix-inline-images ()
    ;;   (when org-inline-image-overlays
    ;;     (org-redisplay-inline-images)))

    ;; for newly-added images inline display

    ;; (add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)
    ;; (add-hook 'before-save-hook 'shk-fix-inline-images)
    ;; (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
    ;; (add-hook 'after-save-hook #'org-redisplay-inline-images)

    ;; -- Customised background color for inline images -----------------------------------------

    ;; (defcustom org-inline-image-background nil
    ;;   "The color used as the default background for inline images.
    ;; When nil, use the default face background."
    ;;   :group 'org
    ;;   :type '(choice color (const nil)))

    ;; FIXME: this cause error to lsp-headerline-breadcrub-mode

    ;; (defun create-image-with-background-color (args)
    ;;   "Specify background color of Org-mode inline image through modify `ARGS'."
    ;;   (let* ((file (car args))
    ;;          (type (cadr args))
    ;;          (data-p (caddr args))
    ;;          (props (cdddr args)))
    ;;     ;; Get this return result style from `create-image'.
    ;;     (append (list file type data-p)
    ;;             (list :background (or org-inline-image-background (face-background 'default)))
    ;;             props)))

    ;; (advice-add 'create-image :filter-args
    ;;             #'create-image-with-background-color)

    ;; ---------------------------------------------------------------------------------------------

    (setq org-ditaa-eps-jar-path "/opt/DitaaEps/DitaaEps.jar"
          org-ditaa-jar-path "/opt/ditaa/ditaa.jar")
    (setq org-plantuml-args '("-headless" "-DRELATIVE_INCLUDE=\".\"")
          org-plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\"")
          org-plantuml-jar-path "/opt/plantuml/plantuml.jar")

    (require 'ox-beamer)
    ;; ox-bibtex requires `bibtex2html' to be installed in your system
    (require 'ox-bibtex)
    (require 'ox-html)

    (require 'ox-odt)
    (setq org-odt-data-dir (concat org-directory "/addon/odt/styles"))

    (require 'ox-latex)
    (setq org-latex-coding-system 'utf-8-unix
          org-latex-table-caption-above nil
          org-latex-tables-column-borders t
          ;; code listing settings, new `minted' is also supported
          org-latex-listings t
          ;; org-latex-listings 'minted
          ;; FIXME: fix the bug of current version
          ;; org-latex-preview-ltxpng-directory "./"
          )

    ;; NOTE: Use org to write the draft of the document, and you can
    ;; fine-tuning of the latex template for the final version.
    (setq org-latex-classes
          '(("beamer" "\\documentclass[presentation,9pt]{beamer}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

            ("article" "\\documentclass[11pt]{article}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

            ("report" "\\documentclass[11pt]{report}"
             ;; ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

            ("book" "\\documentclass[11pt]{book}"
             ;; ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}"    . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

            ("letter" "\\documentclass[11pt]{letter}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

            ("scrartcl" "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

            ("scrreprt" "\\documentclass{scrreprt}"
             ;; ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

            ("scrbook" "\\documentclass{scrbook}"
             ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

            ;; ("beamer" "\\documentclass{beamer}"
            ;;  org-beamer-sectioning)

            ("elegantnote" "\\documentclass{elegantnote}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

            ("elegantpaper" "\\documentclass{elegantpaper}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

            ("elegantbook" "\\documentclass{elegantbook}"
             ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
            ))

    ;; NOTE: The default `inputenc' and `fontenc' packages conflicts
    ;; with `xecjk' and `ctex'. The encoding of the input latex files
    ;; don't need to be set.
    (setq org-latex-default-packages-alist
          '(("" "fixltx2e" nil) ("" "graphicx" t) ("" "longtable" nil)
            ("" "float" nil) ("" "wrapfig" nil) ("" "rotating" nil)
            ("normalem" "ulem" t) ("" "amsmath" t) ("" "textcomp" t)
            ("" "marvosym" t) ("" "wasysym" t) ("" "amssymb" t)
            ("" "hyperref" nil) "\\tolerance=1000"
            ;;("" "amsmath" t) ;; this package cause error, no need
            ))

    ;; NOTE: Alist of packages to be inserted in every LaTeX header.
    ;; These will be inserted after `org-latex-default-packages-alist'.
    (setq org-latex-packages-alist
          '(;; The following 3 packages are required if using `listings'
            ;; ("svgnames, table" "xcolor" t)
            ("" "xcolor" t)
            ("" "listings" t)
            ;; ("" "minted" t)
            ("" "menukeys" t)
            ("" "fontawesome" t)
            ("" "setspace" nil)
            ;; Display various latex-related logos
            ;; ("" "metalogo" t) ;; conflict with tipa package
            ;; ("" "mflogo" t) ("" "texnames" t) ;; not very useful
            ;; ("" "amsmath" nil) ;; this package cause error, no need
            ;; ("" "tikz" nil)
            ;; xelatex font adjustment (by default)
            ;; ("" "fontspec" nil)
            ;; Some extra text markups
            ;; ("normalem" "ulem" t)
            ;; Some figure-related packages
            ;; ("" "rotating" t) ("" "subfig" t)
            ;; Some table-related packages
            ;; ("" "booktabs" t) ("" "longtable" nil) ("" "multirow" t)
            ;; ("" "tabularx" t) ("" "warpcol" t)
            ;; Some document layout/structure-related packages
            ;; ("" "etex" nil) ("" "multicol" nil) ("" "multind" nil)
            ;; ("" "titlesec" nil)
            ))
    )

  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-disable-auto-save 'encrypt
        org-crypt-key "etimecowboy@gmail.com")

  (require 'org-keys)
  (setq org-speed-commands
        (cons '("*" . org-decrypt-entry) org-speed-commands))
  (setq org-speed-commands
        (cons '("&" . org-encrypt-entry) org-speed-commands))

  (require 'org-attach)
  (setq org-speed-commands
        (cons '("A" . org-attach) org-speed-commands))
  (setq org-speed-commands
        (cons '("P" . org-set-property) org-speed-commands))
  (setq org-speed-commands
        (cons '("G" . spacemacs/org-agenda-transient-state/org-agenda-todo) org-speed-commands))

  (require 'org-attach-git)
  (setq org-attach-archive-delete 'query
        org-attach-id-dir "data/"
        org-attach-store-link-p 'attached
        org-attach-use-inheritance t
        org-attach-sync-delete-empty-dir t)
  ;; acctach from dired
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map
                          (kbd "C-c C-x a")
                          #'org-attach-dired-to-subtree)))

  (require 'org-id)
  (setq org-id-link-to-org-use-id 'use-existing)
  )

;; (defun org-extra/pre-init-org-appear ()
;;   (spacemacs|use-package-add-hook org-appear
;;     :post-config
;;     (setq org-appear-autoentities t
;;           org-appear-autolinks 'just-brackets
;;           org-appear-autosubmarkers t
;;           org-appear-delay 0.8
;;           org-appear-inside-latex t)
;;     ))

(defun org-extra/pre-init-org-contacts ()
  (spacemacs|use-package-add-hook org-contacts
    :pre-init
    (setq org-contacts-files '("~/org/roam/contacts.org.gpg"))
    ))

(defun org-extra/pre-init-org-download ()
  (spacemacs|use-package-add-hook org-download
    :post-init
    (add-hook 'dired-mode-hook 'org-download-enable)
    :post-config
    (setq org-download-edit-cmd "krita %s"
          org-download-image-org-width 200
          org-download-method 'attach
          ;; NOTE: scrot only works on X11, not wayland
          ;; org-download-screenshot-method "scrot -s %s"
          org-download-screenshot-method "gnome-screenshot -a -f %s"
          )
    ))

(defun org-extra/pre-init-org-ref ()
  (spacemacs|use-package-add-hook org-ref
    :post-config
    (setq org-ref-open-pdf-function
          (lambda (fpath)
            (start-process "zathura"
                           "*bibtex-zathura*" ;; was "*helm-bibtex-zathura*", changed because helm was removed
                           "/usr/bin/zathura" fpath)))
    ))

(defun org-extra/pre-init-markdown ()
  (spacemacs|use-package-add-hook markdown
    :post-init
    (add-hook 'markdown-mode-hook #'toc-org-mode)
    ))

(defun org-extra/pre-init-org-roam ()
  (spacemacs|use-package-add-hook org-roam
    :post-init
    (setq org-roam-dailies-directory "~/org/dailies"
          org-roam-db-location "~/org/org-roam.db"
          org-roam-directory "~/org/roam")
    (add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list)
    (add-hook 'after-save-hook #'org-redisplay-inline-images)
    ;; FIXME: cause error in embark export
    ;; (advice-add 'org-roam-node-find :before 'org-roam-db-sync)
    ;; (advice-add 'org-roam-node-insert :before 'org-roam-db-sync)
    ;; (advice-add 'org-roam-node-id :before 'org-roam-db-sync)
    ;; (advice-remove 'org-roam-node-insert #'org-roam-db-sync)

    :post-config
    ;; add org fast key
    ;; (define-key org-mode-map (kbd "Z") 'org-roam-extract-subtree)
    (setq org-roam-v2-ack t
          org-roam-db-gc-threshold most-positive-fixnum
          org-roam-completion-everywhere t
          org-roam-list-files-commands '(rg)
          org-roam-mode-sections '(org-roam-backlinks-section
                                   org-roam-reflinks-section
                                   org-roam-unlinked-references-section)
          org-roam-protocol-store-links t)

    (setq org-roam-node-display-template
          (concat "${title:*} " (propertize "${tags:30}" 'face 'org-tag)))

    (setq org-roam-capture-templates
          '(("d" "fleeting (default)"
             entry
             (file "templates/fleeting.org")
             :target (file "~/org/roam/note_inbox.org")
             :prepend t
             :empty-lines 1
             )
            ("l" "literature"
             plain
             (file "templates/literature.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t
             )
            ("p" "permanent"
             plain
             (file "templates/permanent.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t
             )
            ("r" "reference"
             plain
             (file "templates/reference.org")
             :target (file "${slug}.org")
             :unnarrowed t
             :jump-to-captured t)
            ("g" "glossary"
             plain
             (file "templates/glossary.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t)
            ("h" "hub"
             plain
             (file "templates/hub.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t)
            ("j" "project"
             plain
             (file "templates/project.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :clock-keep t
             :unnarrowed t
             :jump-to-captured t)
            ("s" "software"
             plain
             (file "templates/software.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t)
            ("c" "code"
             plain
             (file "templates/code.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t)
            ))

    (setq org-roam-capture-ref-templates
          '(("r" "reference"
             plain
             (file "templates/reference-roam.org")
             :target (file "${slug}.org")
             :immediate-finish t
             :jump-to-captured t
             :empty-lines 1)
            ))

    (setq org-roam-dailies-capture-templates
          '(("d" "default"
             entry
             "* %U %?\n\n%x"
             :target (file+head "%<%Y-%m-%d>.org"
                                "#+title: %<%Y-%m-%d>
* Mind Path

Track my mind of the day, try to focus on the main tasks.

* Media consumption

** [[roam:My playlist]]

#+transclude: [[file:~/org/roam/my_playlist.org::学习音乐]] :only-contents

** [[roam:Bilibili]] 推荐

** [[roam:Youtube]] 内容

* Tasks

Automatically record tasks that are DONE today

{ M-x org-agenda RET d }

* Logs")
             :empty-lines 1
             :unnarrowed t)))

    ;; add org speed keys
    (setq org-speed-commands
          (cons '("x" . org-roam-extract-subtree) org-speed-commands))
    (setq org-speed-commands
          (cons '("X" . org-roam-refile) org-speed-commands))
    (setq org-speed-commands
          (cons '("d" . org-id-get-create) org-speed-commands))
    (setq org-speed-commands
          (cons '("+" . org-roam-alias-add) org-speed-commands))

    ;; NOTE: This global minor mode is too heavy.
    ;; Turn it on/off manually in org-mode { M-m m r r }
    (org-roam-db-autosync-mode 1)
    ))

(defun org-extra/pre-init-org-roam-ui ()
  (spacemacs|use-package-add-hook org-roam-ui
    :post-init
    (advice-add 'org-roam-ui-mode :before 'org-roam-db-sync)
    (spacemacs|diminish org-roam-ui-mode " ⓤ" " u")
    (spacemacs|diminish org-roam-ui-follow-mode)
    ))

(defun org-extra/pre-init-alert ()
  (spacemacs|use-package-add-hook alert
    :post-config
    (setq alert-default-style 'libnotify)
    ))

(defun org-extra/pre-init-org-wild-notifier ()
  (spacemacs|use-package-add-hook org-wild-notifier
    :post-config
    (setq org-wild-notifier-alert-time '(25 15 10 5 3 1)
          org-wild-notifier-keyword-blacklist '("DONE" "CANCELLED" "MARK" "USELESS")
          org-wild-notifier-keyword-whitelist nil
          org-wild-notifier-tags-blacklist '("ARCHIVE"))
    ))

(defun org-extra/init-djvu ()
  (use-package djvu))

;; load org-noter
(defun org-extra/init-org-noter ()
  (use-package org-noter
    :defer t
    :config
    (setq org-noter-auto-save-last-location t
          org-noter-notes-search-path '("~/doc")
          org-noter-always-create-frame nil
          org-noter-separate-notes-from-heading t)
    (require 'org-noter-pdftools)
    ))

;; load org-pdftools
(defun org-extra/init-org-pdftools ()
  (use-package org-pdftools
    :hook (org-mode . org-pdftools-setup-link)))

;; load org-noter-pdftools
(defun org-extra/init-org-noter-pdftools ()
  (use-package org-noter-pdftools
    :after org-noter
    :config
    (setq org-noter-pdftools-use-org-id t)
    (setq org-noter-pdftools-use-unique-org-id t)
    ;; Add a function to ensure precise note is inserted
    (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
      (interactive "P")
      (org-noter--with-valid-session
       (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                     (not org-noter-insert-note-no-questions)
                                                   org-noter-insert-note-no-questions))
             (org-pdftools-use-isearch-link t)
             (org-pdftools-use-freepointer-annot t))
         (org-noter-insert-note (org-noter--get-precise-info)))))

    ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
    (defun org-noter-set-start-location (&optional arg)
      "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
      (interactive "P")
      (org-noter--with-valid-session
       (let ((inhibit-read-only t)
             (ast (org-noter--parse-root))
             (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
         (with-current-buffer (org-noter--session-notes-buffer session)
           (org-with-wide-buffer
            (goto-char (org-element-property :begin ast))
            (if arg
                (org-entry-delete nil org-noter-property-note-location)
              (org-entry-put nil org-noter-property-note-location
                             (org-noter--pretty-print-location location))))))))

    (with-eval-after-load 'pdf-annot
      (add-hook 'pdf-annot-activate-handler-functions
                #'org-noter-pdftools-jump-to-note))))

(defun org-extra/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    ;; :after org-roam
    ))

;; load org-fc
(defun org-extra/init-org-fc ()
  (use-package org-fc
    :init
    (spacemacs|define-transient-state org-fc
      :title "org-fc transient state"
      :doc "
^org-fc^
^^^^^^^^----------------------------------------------------
[_t_] Init Type         [_u_] Update Card
[_r_] Start Review      [_s_] Suspend/Unsuspend
[_m_] Dashboard         [_a_] Audio Control
[_q_] quit
"
      :bindings
      ("t" spacemacs/org-fc-type-transient-state/body :exit t)
      ("u" org-fc-update)
      ("r" org-fc-review :exit t)
      ("s" spacemacs/org-fc-suspend-transient-state/body :exit t)
      ("m" org-fc-dashboard :exit t)
      ("a" spacemacs/org-fc-audio-transient-state/body :exit t)
      ("q" nil :exit t))

    (spacemacs|define-transient-state org-fc-type
      :title "org-fc-type transient state"
      :doc "
^Init Type^
^^^^^^^^----------------------------------------------------
[_n_] Normal  [_d_] Double  [_t_] Text Input  [_c_] Deletion
[_e_] Enum    [_s_] Single  [_x_] Context     [_v_] Vocab
[_q_] quit
"
      :bindings
      ("n" org-fc-type-normal-init :exit t)
      ("d" org-fc-type-double-init :exit t)
      ("t" org-fc-type-text-input-init :exit t)
      ("c" (org-fc-type-cloze-init 'deletion) :exit t)
      ("e" (org-fc-type-cloze-init 'enumeration) :exit t)
      ("s" (org-fc-type-cloze-init 'single) :exit t)
      ("x" (org-fc-type-cloze-init 'context) :exit t)
      ("v" org-fc-type-vocab-init :exit t)
      ("q" nil :exit t))

    (spacemacs|define-transient-state org-fc-suspend
      :title "org-fc-suspend transient state"
      :doc "
^Suspend/Unsuspend^
^^^^^^^^----------------------------------------
[_c_/_C_] Card  [_t_/_T_] Tree  [_b_/_B_] Buffer
[_q_] quit
"
      :bindings
      ("c" org-fc-suspend-card)
      ("C" org-fc-unsuspend-card)
      ("t" org-fc-suspend-tree)
      ("T" org-fc-unsuspend-tree)
      ("b" org-fc-suspend-buffer)
      ("B" org-fc-unsuspend-buffer)
      ("q" nil :exit t))

    (spacemacs|define-transient-state org-fc-audio
      :title "org-fc-suspend transient state"
      :doc "
^Audio Control^
^^^^^^^^----------------------------------------
[_p_] Play  [_r_] Replay  [_s_] Slow Replay
[_q_] quit
"
      :bindings
      ("p" org-fc-audio-play)
      ("r" org-fc-audio-replay)
      ("s" org-fc-audio-replay-slow)
      ("q" nil :exit t))

    :config
    (require 'org-fc-audio)
    (require 'org-fc-keymap-hint)
    ;; (require 'org-fc-hydra)
    (require 'org-fc-type-vocab)
    (setq org-fc-directories '("~/org/roam"))
    ;; add org speed keys
    (setq org-speed-commands
          (cons '("h" . spacemacs/org-fc-transient-state/body) org-speed-commands))
    (setq org-speed-commands
          (cons '("H" . spacemacs/org-fc-suspend-transient-state/body) org-speed-commands))
    ;; ;; add more org-fc hydras
    ;; (defhydra org-fc-suspend ()
    ;;   ("s" org-fc-suspend-card "Suspend Card" :exit t)
    ;;   ("S" org-fc-unsuspend-card "Unsuspend Card" :exit t)
    ;;   ("t" org-fc-suspend-tree "Suspend Tree" :exit t)
    ;;   ("T" org-fc-unsuspend-tree "Unsuspend Tree" :exit t)
    ;;   ("b" org-fc-suspend-buffer "Suspend Buffer" :exit t)
    ;;   ("B" org-fc-unsuspend-buffer "Unsuspend Buffer" :exit t)
    ;;   ("q" nil "Quit" :exit t))
    ;; (defhydra org-fc-audio-control ()
    ;;   ("p" org-fc-audio-play "Play")
    ;;   ("r" org-fc-audio-replay "Replay")
    ;;   ("s" org-fc-audio-replay-slow "Slow replay")
    ;;   ("q" nil "Quit" :exit t))
    ;; ;; add org-fc hydra heads by `defhydra+' macro
    ;; ;; REF: https://github.com/abo-abo/hydra/issues/185
    ;; (defhydra+ org-fc-hydra-type ()
    ;;   ("v" org-fc-type-vocab-init "Vocab" :exit t))
    ;; (defhydra+ org-fc-hydra ()
    ;;   ("s" org-fc-suspend/body "Suspend/Unsuspend" :exit t)
    ;;   ("a" org-fc-audio-control/body "Audio Control" :exit t))
    ))

;; load org-fragtog
(defun org-extra/init-org-fragtog ()
  (use-package org-fragtog
    :hook
    (org-mode . (lambda ()
                  (if (display-graphic-p)
                      (org-fragtog-mode 1)
                    (org-fragtog-mode -1))))
    :custom
    (org-fragtog-ignore-predicates
     '(org-at-table-p org-at-table\.el-p org-at-block-p org-at-heading-p))
    (org-fragtog-preview-delay 0.5)
    ))

;; load org-web-tools
(defun org-extra/init-org-web-tools ()
  (use-package org-web-tools
    :config
    (setq org-web-tools-archive-fn 'org-web-tools-archive--wget-tar
          org-web-tools-attach-archive-max-attempts 5
          org-web-tools-attach-archive-retry 10
          org-web-tools-attach-archive-retry-fallback nil)
    (setq org-web-tools-archive-wget-html-only-options
          '("--execute" "robots=off"
            "--adjust-extension"
            "--timestamping"
            "--no-directories"))
    (setq org-web-tools-archive-wget-options
          '("--ignore-tags=script,iframe"
            "--reject=eot,ttf,svg,otf,*.woff*"
            "--execute" "robots=off"
            "--adjust-extension"
            "--span-hosts"
            "--convert-links"
            "--page-requisites"
            "--timestamping"
            "--no-directories"))
    ;; Add speed keys
    (setq org-speed-commands
          (cons '("T" . org-web-tools-archive-attach)
                org-speed-commands))
    (setq org-speed-commands
          (cons '("V" . org-web-tools-archive-view)
                org-speed-commands))
    ))

;; load org-auto-tangle
(defun org-extra/init-org-auto-tangle ()
  (use-package org-auto-tangle
    :defer t
    :hook
    (org-mode . org-auto-tangle-mode)
    :init
    (spacemacs|diminish org-auto-tangle-mode " ⓣ" " org-a-t")
    ))

(defun org-extra/post-init-org-modern ()

  ;; Remove the hook that was added in
  ;;
  ;; <find-function-other-window 'org/init-org-modern>
  ;;
  ;; check `xy/adapt-org-config' function that adds and remove hooks according
  ;; to the environment (GUI or terminal)
  ;;
  ;; "funcs.el#(defun xy/adapt-org-config"
  ;;
  (remove-hook 'org-mode-hook 'org-modern-mode)
  ;; (remove-hook 'org-agenda-finalize-hook 'org-modern-agenda)

  ;; (setq org-modern-todo nil)
  (setq org-modern-hide-stars 'leading
        org-modern-star '("✿" "✳" "✸" "◉" "○" "◈" "◇")
        ;; org-modern-block-name '("▿" . "▵")
        org-modern-block-name '("▽" . "△")
        org-modern-todo-faces
        '(("TODO" :background "black" :foreground "dark orange" :weight bold)
          ("SOMEDAY" :background "black" :foreground "slate grey" :weight bold)
          ("NEXT" :background "black" :foreground "magenta" :weight bold)
          ("STARTED" :background "black" :foreground "red" :weight bold)
          ("WAITING" :background "black" :foreground "yellow" :weight bold)
          ("DONE" :background "black" :foreground "green" :weight bold)
          ("CANCELLED" :background "black" :foreground "cyan" :weight bold)
          ("NEW" :background "black" :foreground "dark orange" :weight bold)
          ("REVIEW" :background "black" :foreground "magenta" :weight bold)
          ("MARK" :background "black" :foreground "red" :weight bold)
          ("USELESS" :background "black" :foreground "cyan" :weight bold)
          (t :background "black" :foreground "dark orange" :weight bold)))
  )

;; load ob-async
(defun org-extra/init-ob-async ()
  (use-package ob-async
    ;; :init
    ;; (setq ob-async-reload-p nil)
    ;; (defun reload-ob-async ()
    ;;   (when (not ob-async-reload-p)
    ;;     (load-library "ob-async")
    ;;     (setq ob-async-reload-p t)
    ;;     (message "Reload ob-async.el")))
    ;; :hook
    ;; (org-mode . reload-ob-async)
    :config
    ;; NOTE: ob-ipython is replaced by jupyter
    ;; (setq ob-async-no-async-languages-alist '("ipython"))
    (setq ob-async-no-async-languages-alist '("jupyter-python"))

    ;; FIXME: it is strange that `ob-async' requires a reload to work
    ;;
    ;; Tried solution 1 - Failed
    ;;
    ;; Configure `ob-async-pre-execute-src-block-hook' to load `ob-async'
    ;;
    ;; (setq ob-async-pre-execute-src-block-hook
    ;;       (lambda () (load-library "ob-async")))
    ;;
    ;; Tried solution 2 - Passed
    ;;
    ;; Add a hook to org-mode to load `ob-async' once.
    ;;
    ;; Fix error in setq in the code sent to the background emacs process.
    ;;
    ;; ;; (setq org-babel-hide-result-overlays (#<overlay from 79 to 79>))
    ;;
    ;; See REF:
    ;;   - https://github.com/astahlman/ob-async/issues/75
    ;;   - https://www.reddit.com/r/emacs/comments/v2p4q9/orgbabel_problems_with_obasync/
    ;;
    ;; (defun no-hide-overlays (orig-fun &rest args)
    ;;   (setq org-babel-hide-result-overlays nil))
    ;;
    ;; (advice-add 'ob-async-org-babel-execute-src-block
    ;;             :before #'no-hide-overlays)
    ))

;; load mathpix, requires a paid account
;; (defun org-extra/init-mathpix ()
;;   (use-package mathpix
;;     :custom ((mathpix-app-id "app-id")
;;              (mathpix-app-key "app-key"))
;;     :bind
;;     ("C-x m" . mathpix-screenshot)
;;     ))

;; ;; load org-inline-anim
;; (defun org-extra/init-org-inline-anim ()
;;   (use-package org-inline-anim
;;     :hook
;;     (org-mode . org-inline-anim-mode)))

;; ;; load org-special-block-extras
;; (defun org-extra/init-org-special-block-extras ()
;;   (use-package org-special-block-extras
;;     :hook (org-mode . org-special-block-extras-mode)
;;     ;; All relevant Lisp functions are prefixed ‘o-’; e.g., `o-docs-insert'.
;;     ;; :custom
;;     ;; (o-docs-libraries
;;     ;;  '("~/org-special-block-extras/documentation.org")
;;     ;;  "The places where I keep my ‘#+documentation’")
;;     :config
;;     (org-defblock src (lang nil) (title nil exports nil file nil)
;;                   "Fold-away all ‘src’ blocks as ‘<details>’ HTML export.
;; If a block has a ‘:title’, use that to title the ‘<details>’."
;;                   (format "<details> <summary> %s </summary> <pre> %s </pre></details>"
;;                           (or title (concat "Details; " lang))
;;                           raw-contents))
;;     ))

;; load ob-ipython
;; (defun org-extra/init-ob-ipython ()
;;   (use-package ob-ipython
;;     :defer t
;;     :after ob
;;     :config
;;     (setq ob-ipython-command "ipython3")
;;     ))

;;; packages.el ends here
