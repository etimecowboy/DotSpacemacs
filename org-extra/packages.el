;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- org-extra layer packages file for Spacemacs.
;; Time-stamp: <2024-10-18 Fri 05:10:33 GMT by xin on tufg>
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
    ;;----- added packages
    djvu
    org-noter
    org-pdftools
    org-noter-pdftools
    org-fragtog
    (org-roam-bibtex :requires org-roam)
    org-web-tools
    org-auto-tangle
    engrave-faces
    (ob-async :location local) ;; NOTE: Use patched version.
    (org-ql :location (recipe :fetcher github :repo "alphapapa/org-ql"
                              :files (:defaults (:exclude "helm-org-ql.el"))))
    (embark-org-roam :location (recipe :fetcher github
                                       :repo "bramadams/embark-org-roam")
                     :require (embark org-roam))
    org-timeblock
    literate-calc-mode
    ;; (org-node :location (recipe :fetcher github :repo "meedstrom/org-node"))

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
    ;; org-fc ;; FIXME: package is removed from elpa, do you really nead it?
    ))

(defun org-extra/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :pre-init
    (setq org-directory "~/org/"
          org-default-notes-file "~/org/notes.org")

    (setq org-modules '(ol-bbdb ol-bibtex org-crypt ol-docview ol-doi ol-eww ;; org-ctags
                                ol-gnus org-id ol-info org-inlinetask ol-irc org-habit
                                ol-mhe org-mouse org-protocol ol-rmail ol-w3m ol-eshell
                                ol-bookmark ol-elisp-symbol ol-man ;; ol-git-link org-eval
                                org-toc
                                ))

    ;; fix error "org-element--list-struct: Tab width in Org files must be 8,
    ;; not 2. Please adjust your ‘tab-width’ settings for Org mode" when
    ;; embark-export org-roam nodes.
    ;; (defun xy/set-local-tab-width (&optional n)
    ;;   "Set local tab width to n."
    ;;   (let ((n (or n 8)))
    ;;     (add-hook 'after-change-major-mode-hook :local
    ;;               (setq-local tab-width n))))
    ;; ;; (add-hook 'org-mode-hook #'xy/set-local-tab-width)
    ;; (spacemacs/add-to-hook 'org-mode-hook ')
    :post-init
    (add-hook 'after-save-hook #'org-redisplay-inline-images)
    ;; (add-hook 'org-mode-hook #'toc-org-mode)
    (add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list)
    ;; (add-hook 'org-mode-hook #'xy/adapt-org-config)
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
    ;; Make sure directory is correct again
    (setq org-directory "~/org/"
          org-default-notes-file "~/org/notes.org")

    ;; ---- Basic customization ------------------------------------------------

    (setq org-startup-indented nil)

    ;; ---- org faces ----------------------------------------------------------
    ;; (xy/org-faces-config)
    ;; (require 'org-faces)
    ;; (set-face-attribute 'fixed-pitch nil :font xy:org-fixed-width-font)
    ;; (set-face-attribute 'variable-pitch nil :font xy:org-variable-width-font)
    ;; (set-face-attribute 'org-default nil :inherit 'variable-pitch)
    ;; (set-face-attribute 'org-document-title nil :inherit '(variable-pitch bold))
    ;; (set-face-attribute 'org-document-info nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-table-header nil :inherit '(fixed-pitch bold))
    ;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-code nil :inherit 'fixed-pitch :extend t)
    ;; (set-face-attribute 'org-quote nil :inherit '(variable-pitch bold))
    ;; (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-date nil :inherit '(fixed-pitch bold))
    ;; (set-face-attribute 'org-drawer nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-sexp-date nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-clock-overlay nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-date-selected nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-block-begin-line nil :inherit '(fixed-pitch bold)
    ;;                     :overline nil :underline t :extend t)
    ;; (set-face-attribute 'org-block-end-line nil :inherit '(fixed-pitch bold)
    ;;                     :overline t :underline nil :extend t)

    ;; ---- org fast keys ------------------------------------------------------

    ;; REF: https://www.youtube.com/watch?v=v-jLg1VaYzo

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

    ;; ---- org links ----------------------------------------------------------

    (setq org-link-frame-setup
          '((vm . vm-visit-folder-other-frame)
            (vm-imap . vm-visit-imap-folder-other-frame)
            (gnus . org-gnus-no-new-news)
            (file . find-file)
            (wl . wl-other-frame)))

    ;; ---- org footnotes ------------------------------------------------------

    (setq org-footnote-section nil
          org-footnote-auto-adjust t
          org-footnote-auto-label t)

    ;; ---- org code blocks ----------------------------------------------------

    (setq org-edit-src-turn-on-auto-save t)

    (setq org-src-ask-before-returning-to-edit-buffer nil
          org-src-preserve-indentation t)

    (add-list-to-list 'org-src-lang-modes
                      '(("latex" . latex )
                        ("emacs-lisp" . emacs-lisp )
                        ("python" . python)
                        ("jupyter" . python)
                        ))
    (delq nil (delete-dups org-src-lang-modes))

    ;; REF: https://stackoverflow.com/questions/44811679/orgmode-change-code-block-background-color
    (setq org-src-block-faces
          '(;; compiled languages
            ("C" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("C++" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("go" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("rust" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ;; script languages
            ("emacs-lisp" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("elisp" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("python" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("perl" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("ruby" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("matlab" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("r" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ;; shell languages
            ("shell" (:inherit fixed-pitch :background "navy" :extend t))
            ("bash" (:inherit fixed-pitch :background "navy" :extend t))
            ("sh" (:inherit fixed-pitch :background "navy" :extend t))
            ("tmux" (:inherit fixed-pitch :background "navy" :extend t))
            ;; config languages
            ("conf" (:inherit fixed-pitch :background "SlateGray4" :extend t))
            ("yaml" (:inherit fixed-pitch :background "SlateGray4" :extend t))
            ("json" (:inherit fixed-pitch :background "SlateGray4" :extend t))
            ("lua" (:inherit fixed-pitch :background "SlateGray4" :extend t))
            ;; markup languages
            ("org" (:inherit fixed-pitch :background "gray30" :extend t))
            ("latex" (:inherit fixed-pitch :background "gray30" :extend t))
            ("markdown" (:inherit fixed-pitch :background "gray30" :extend t))
            ;; graphic description languages
            ("plantuml" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("graphviz" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ("dot" (:inherit fixed-pitch :background "unspecified-bg" :extend t))
            ))

    ;; ---- GTD related --------------------------------------------------------

    ;; -------- properties --------
    (setq org-global-properties
          '(("POMODORO_ALL" . "0 1 2 3 4 5")
            ("SCORE_ALL" . "0 1 2 3 4 5")))
    (setq org-use-property-inheritance
          "header-args\\|shebang\\|session\\|DIR\\|dir")

    ;; -------- tags --------
    (setq org-tag-persistent-alist
          '(
            ;; states of writing
            (:startgroup) ("INCOMING" . ?0) ("DRAFTING" . ?1) ("FINISHED" . ?2)
            ("STACKED" . ?3) ("REVISING" . ?4) ("PUBLISHED" . ?5)
            ("REDIRECTED" . ?9)  (:endgroup)

            ;; My processing result
            (:startgroup) ("UNCERTAIN" . ?u) ("VERIFIED" . ?V) ("PASSED" . ?K)
            ("FAILED" . ?X) ("ADOPTED" . ?O) ("DEPRECATED" . ?d) (:endgroup)

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
            (:startgrouptag) ("code" . ?c) ("config" . ?o) ("template" . ?t)
            ("data" . ?y) ("tip" . ?i) ("example" . ?e) ("test" . ?k)
            ("vocabulary" . ?v) ("quotation" . ?q) (:endgrouptag)

            ;; categories defined by fast reading (meta learning, for literature
            ;; and permanent notes)
            (:startgroup) ("concept" . ?x) ("fact" . ?a) ("procedure" . ?m)
            (:endgroup)

            ;; PARA method
            (:startgroup) ("PROJECT" . ?P) ("AREA" . ?A) ("RESOURCE" . ?R)
            ("ARCHIVE" . ?Z) (:endgroup)

            ;; tags used by org and org extensions
            (:startgrouptag) ("FLAGGED" . ?F) ("ATTACH" . ?H)
            ("crypt" . ?Y) ("noexport" . ?N) ("TOC" . ?i) ;; ("fc" . ?~)
            (:endgrouptag)

            ;; GTD state
            (:startgrouptag) ("repeat" . ?*) ("CONFIDENTIAL" . ?C)
            (:endgrouptag)

            ;; (:startgrouptag) ("action" . ?a) ("hidden" . ?i)
            ;; ("status" . ?s) (:endgrouptag)
            ))

    ;; "action" "status" "hidden" "publication" "code" "vocabulary" "quotation"
    (setq org-use-tag-inheritance '("PROJECT" "AREA" "RESOURCE" "ARCHIVE" "ATTACH"))

    ;; -------- logging --------
    (setq org-log-done 'time
          org-log-into-drawer t
          org-log-redeadline 'note
          org-log-refile 'time
          org-log-reschedule 'time
          org-log-state-notes-insert-after-drawers t)

    ;; -------- refile --------
    (setq org-refile-targets '((nil :maxlevel . 4)
                               (org-agenda-files :maxlevel . 4))
          org-refile-use-outline-path 'title)

    ;; -------- projects --------
    (setq org-stuck-projects '("+PROJECT/-SOMEDAY-DONE" ("NEXT" "STARTED")))

    ;; -------- TODO keywords --------
    (setq org-treat-S-cursor-todo-selection-as-state-change nil
          org-treat-insert-todo-heading-as-state-change t
          org-enforce-todo-checkbox-dependencies t
          org-enforce-todo-dependencies t)

    (setq org-todo-keywords
          '((sequence "TODO(t)" "SOMEDAY(x)" "NEXT(n)"
                      "STARTED(s!)" "WAITING(w!)" "|"
                      "DONE(d!)" "CANCELLED(c@/!)")
            (sequence "NEW(a)" "REVIEW(r!)" "|"
                      "MARK(m!)" "USELESS(u!)")))

    (setq org-after-todo-state-change-hook
          '((lambda nil
              (when
                  (equal org-state "DONE")
                (xy/org-roam-log-todo-today)))
            (closure
                (t)
                nil
              (if (or (string= org-state "SOMEDAY") (string= org-state "TODO"))
                  (org-remove-timestamp-with-keyword org-scheduled-string))
              (if (string= org-state "NEXT")
                  (progn
                    (org-deadline nil "+0")
                    (xy/org-roam-log-todo-today)
                    (tab-bar-mode 1)
                    (tab-bar-new-tab)
                    ;; (let ((mm major-mode))
                    ;;   (pcase mm
                    ;;     (org-mode (tab-bar-rename-tab
                    ;;                (concat "Task: "
                    ;;                        (org-get-heading t t nil t))))
                    ;;     (org-agenda-mode (tab-bar-rename-tab ;; shadowed by 'org-mode
                    ;;                       (concat "Task: "
                    ;;                               (buffer-substring-no-properties
                    ;;                                (line-beginning-position)
                    ;;                                (line-end-position)))))
                    ;;     (_ (error "Invalid mojor-mode."))))))
                    (let ((mm major-mode))
                      (when (eq mm 'org-mode)
                        (tab-bar-rename-tab (concat "Task: "
                                                    (org-get-heading t t nil t))))
                      (when (eq mm 'org-agenda-mode)
                        (tab-bar-rename-tab (buffer-substring-no-properties
                                             (line-beginning-position)
                                             (line-end-position)))))))

              (if (string= org-state "DONE")
                  (alert "WELL DONE" :title "Agenda" :category 'Emacs :severity 'trivial))
              ;; NOTE: I would like to have a general `REVIEW' state instead of
              ;; fc specific flashcard.
              ;;
              ;; (if
              ;;     (string= org-state "REVIEW")
              ;;     (org-fc-type-vocab-init))
              (if
                  (string= org-state "MARK")
                  (progn
                    (org-roam-extract-subtree)))
              (if
                  (string= org-state "USELESS")
                  (progn
                    (org-roam-refile) ;; subtree goes to "Useless Stuff"
                    ))
              )))

    (defun xy/org-roam-log-todo-today ()
      (let ((org-refile-keep t) ;; Set this to t to keep the original!
            (org-roam-dailies-capture-templates
             '(("a" "archive" entry "%?"
                :target (file+head+olp "%<%Y-%m-%d>.org"
                                       "#+title: %<%Y-%m-%d>
#+filetags: :dailies:

* Mind path

* Notes

* Log

** %U Diary was created.

* Workspace
" ("Log")))))
            (org-after-refile-insert-hook #'save-buffer)
            today-file
            pos)
        (save-window-excursion
          (org-roam-dailies--capture (current-time) t)
          (setq today-file (buffer-file-name))
          (setq pos (point)))

        ;; Only refile if the target file is different than the current file
        (unless (equal (file-truename today-file)
                       (file-truename (buffer-file-name)))
          (org-refile nil nil (list "Log" today-file nil pos)))))

    ;; -------- capture --------
    (setq org-reverse-note-order t)
    (setq org-capture-templates
          '(
            ("t" "Task" entry
             (file "~/org/roam/task_inbox.org")
             (file "templates/task.org")
             :prepend t :empty-lines 2 :clock-keep t)
            ("b" "Bookmark" entry
             (file "~/org/roam/bookmark_inbox.org")
             (file "templates/bookmark.org")
             :prepend t :empty-lines 2 :clock-keep t)
            ("f" "Fleeting Note" entry
             (file "~/org/roam/note_inbox.org")
             (file "templates/fleeting-note.org")
             :prepend t :empty-lines 2 :clock-keep t)
            ("l" "Literature Note" entry
             (file "~/org/roam/note_inbox.org")
             (file "templates/literature-note.org")
             :prepend t :empty-lines 2 :clock-keep t)
            ("p" "Permanent Note" entry
             (file "~/org/roam/note_inbox.org")
             (file "templates/permanent-note.org")
             :prepend t :empty-lines 2 :clock-keep t)
            ("g" "Glossary Note" entry
             (file "~/org/roam/note_inbox.org")
             (file "templates/glossary-note.org")
             :prepend t :empty-lines 2 :clock-keep t)
            ("s" "Software Note" entry
             (file "~/org/roam/note_inbox.org")
             (file "templates/software-note.org")
             :prepend t :empty-lines 2 :clock-keep t)
            ("v" "Vocabulary" entry
             (file "~/org/roam/vocabulary_inbox.org")
             (file "templates/vocab.org")
             :prepend t :empty-lines 2 :clock-keep t)
            ("c" "Contacts" entry
             (file "~/org/roam/contacts.org.gpg")
             (file "templates/contact.org")
             :prepend t :empty-lines 2 :clock-keep t)
            ("x" "Password" entry
             (file "~/org/roam/passwords.org.gpg")
             (file "templates/password.org")
             :prepend t :empty-lines 2 :clock-keep t)
            ))

    ;; -------- archive --------
    (setq org-archive-save-context-info
          '(time file category todo priority itags olpath ltags))

    ;; -------- clocking --------
    (setq org-columns-default-format
          "%CATEGORY(Cat.) %PRIORITY(Pri.) %6TODO(State) %35ITEM(Details) %ALLTAGS(Tags) %5NUM_POMODORO(Plan){:} %6CLOCKSUM(Clock){Total} %SCORE(SCORE)")

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

    ;; -------- agenda --------
    (setq org-agenda-block-separator 9473
          org-agenda-dim-blocked-tasks nil
          org-agenda-exporter-settings '((ps-number-of-columns 2)
                                         (ps-landscape-mode t)
                                         (org-agenda-add-entry-text-maxlines 5)
                                         (htmlize-output-type 'css))
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-sorting-strategy '((agenda time-up category-keep priority-down todo-state-up)
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

    (setq org-agenda-custom-commands
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

    ;; -------- Previewing LaTeX fragements -----------------------------------
    ;;
    ;; NOTE: These variables are defined in `org.el' and `org-compat.el' for
    ;; general usage. For example, `ox-latex.el', `ob-latex.el', depends on
    ;; them.

    ;; (setq org-latex-preview-ltxpng-directory "./")

    ;; For debugging purpose, Let's singal an error when image creation of LaTeX
    ;; fragments fails
    (setq org-format-latex-signal-error nil)

    ;; NOTE: This variable is obsolete since 9.0; use
    ;; ‘org-preview-latex-default-process’ instead.
    ;;
    ;; (setq org-latex-create-formula-image-program 'imagemagick)

    (setq org-preview-latex-default-process 'png)

    ;; Customized processing paths for previewing latex fragments
    (add-list-to-list 'org-preview-latex-process-alist
                      '(;; (png :programs ("xelatex" "convert")
                        ;;      :description "pdf > png"
                        ;;      :message "you need to install the programs: xelatex and imagemagick."
                        ;;      :image-input-type "pdf"
                        ;;      :image-output-type "png"
                        ;;      :image-size-adjust (1.0 . 1.0)
                        ;;      :latex-compiler ("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
                        ;;      :image-converter ("convert -density 150 -trim -antialias %f -quality 100 %O")
                        ;;      )
                        (png :programs ("latexmk" "convert")
                             :description "pdf > png"
                             :message "you need to install the programs: latexmk and imagemagick."
                             :image-input-type "pdf"
                             :image-output-type "png"
                             :image-size-adjust (1.0 . 1.0)
                             :latex-compiler ("latexmk -norc -pdfxe -silent -shell-escape -interaction=nonstopmode -output-directory=%o %f")
                             ;; "latexmk -shell-escape -interaction nonstopmode -output-directory %o %f")
                             :image-converter ("convert -density 150 -trim -antialias %f -quality 100 %O")
                             )
                        (svg :programs ("xelatex" "dvisvgm")
                             :description "xdv > svg"
                             :message "you need to install the programs: xelatex and dvisvgm."
                             :image-input-type "xdv"
                             :image-output-type "svg"
                             :image-size-adjust (1.7 . 1.5)
                             :latex-compiler ("xelatex -no-pdf -shell-escape -interaction nonstopmode -output-directory %o %f")
                             :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O") ;; default
                             ;; :image-converter ("dvisvgm %f -n -b min -c %S -o %O")
                             )
                        ))
    (setq org-preview-latex-process-alist (asoc-uniq org-preview-latex-process-alist))

    ;; Default value
    ;;
    ;; (setq org-preview-latex-process-alist
    ;;       '((dvipng :programs ("latex" "dvipng")
    ;;                 :description "dvi > png"
    ;;                 :message "you need to install the programs: latex and dvipng."
    ;;                 :image-input-type "dvi"
    ;;                 :image-output-type "png"
    ;;                 :image-size-adjust (1.0 . 1.0)
    ;;                 :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
    ;;                 :image-converter ("dvipng -D %D -T tight -o %O %f")
    ;;                 :transparent-image-converter ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
    ;;         (dvisvgm :programs ("latex" "dvisvgm")
    ;;                  :description "dvi > svg"
    ;;                  :message "you need to install the programs: latex and dvisvgm."
    ;;                  :image-input-type "dvi"
    ;;                  :image-output-type "svg"
    ;;                  :image-size-adjust (1.7 . 1.5)
    ;;                  :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
    ;;                  :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
    ;;         (imagemagick :programs ("latex" "convert")
    ;;                      :description "pdf > png"
    ;;                      :message "you need to install the programs: latex and imagemagick."
    ;;                      :image-input-type "pdf"
    ;;                      :image-output-type "png"
    ;;                      :image-size-adjust (1.0 . 1.0)
    ;;                      :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
    ;;                      :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))

    ;; NOTE: working backup
    ;;
    ;; (setq org-preview-latex-process-alist
    ;;       '((dvipng
    ;;          :programs ("latex" "dvipng")
    ;;          :description "dvi > png"
    ;;          :message "you need to install the programs: latex and dvipng."
    ;;          :image-input-type "dvi"
    ;;          :image-output-type "png"
    ;;          :image-size-adjust (1.0 . 1.0)
    ;;          ;; :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f") ;; default
    ;;          :latex-compiler ("latex -interaction=nonstopmode -output-directory=%o %f")
    ;;          :image-converter ("dvipng -D %D -T tight -o %O %f") ;; default
    ;;          :transparent-image-converter ("dvipng -D %D -T tight -bg Transparent -o %O %f") ;; default
    ;;          )
    ;;         (dvisvgm
    ;;          :programs ("xelatex" "dvisvgm")
    ;;          :description "xdv > svg"
    ;;          :message "you need to install the programs: xelatex and dvisvgm."
    ;;          :image-input-type "xdv"
    ;;          :image-output-type "svg"
    ;;          :image-size-adjust (1.7 . 1.5)
    ;;          ;; :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f") ;; default
    ;;          :latex-compiler ("xelatex -no-pdf -shell-escape -interaction=nonstopmode -output-directory=%o %f")
    ;;          :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O") ;; default
    ;;          ;; :image-converter ("dvisvgm %f -n -b min -c %S -o %O")
    ;;          )
    ;;         (imagemagick
    ;;          :programs ("xelatex" "convert")
    ;;          :description "pdf > png"
    ;;          :message "you need to install the programs: xelatex and imagemagick."
    ;;          :image-input-type "pdf"
    ;;          :image-output-type "png"
    ;;          :image-size-adjust (1.0 . 1.0)
    ;;          ;; :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f") ;; default
    ;;          ;; :latex-compiler ("latexmk -norc -silent -shell-escape -interaction=nonstopmode -pdfxe -output-directory=%o %f")
    ;;          ;; :latex-compiler ("latexmk -norc -pdfxe -shell-escape -output-directory=%o %f")
    ;;          ;; :latex-compiler ("latexmk -norc -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")
    ;;          :latex-compiler ("xelatex -shell-escape -interaction=nonstopmode -output-directory=%o %f")
    ;;          ;; :image-converter ("convert -density %D -trim -antialias %f -quality 100 -colorspace RGB %O")
    ;;          :image-converter ("convert -density 96 -trim -antialias %f -quality 100 %O"))
    ;;         ))


    ;; default value
    ;; (setq org-format-latex-options '(:foreground default
    ;;                                              :background default
    ;;                                              :scale 1.0
    ;;                                              :html-foreground "Black"
    ;;                                              :html-background "Transparent"
    ;;                                              :html-scale 1.0
    ;;                                              :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

    ;; Backup:
    ;; (setq org-format-latex-options
    ;;       '(:foreground default
    ;;                     :background default
    ;;                     :foreground default
    ;;                     :scale 1.0
    ;;                     :html-foreground "Black"
    ;;                     :html-background "Transparent"
    ;;                     :html-scale 1.0
    ;;                     :matchers ("$1" "$" "$$" "\\(" "\\[" "\\begin{")))

    ;; LaTeX header that will be used when processing a fragment
    (setq org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
[DEFAULT-PACKAGES]
[PACKAGES]
% ==== Page settings ================================
\\pagestyle{empty}
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
%
% ==== Font settings ================================
%
% -------- xeCJK ---------
% -------- Same font as Emacs
\\setmainfont{Sarasa Mono SC Nerd Font}
\\setCJKmainfont{Sarasa Mono SC Nerd Font}
%
% -------- unicode-math --
% -------- STIX Two Math font for math
\\setmathfont{STIXTwoMath-Regular.otf}
% ==== Other pacakges ===============================
%
% Including options set for preview only
%
% -------- minted ---------
\\usepackage[outputdir=/tmp]{minted}
% -------- xcolor ---------
\\usepackage[table]{xcolor}
")

    ;; NOTE: `org-format-latex-header' extra packages.
    ;; Deleted for faster previewing
    ;; % -------- fontawesome -----
    ;; % \\usepackage{fontawesome}
    ;; % -------- metalogo --------
    ;; % \\usepackage{metalogo}
    ;; % -------- menukeys --------
    ;; % \\usepackage{menukeys}
    ;; % \\renewmenumacro{\\keys}[+]{roundedkeys}
    ;; % \\renewmenumacro{\\menu}[>]{roundedmenus}
    ;; % \\renewmenumacro{\\directory}[/]{hyphenatepathswithblackfolder}
    ;; % -------- minted --------
    ;; % \\usepackage[outputdir=/tmp/]{minted}
    ;; % -------- bm ------------
    ;; % \\usepackage{bm}
    ;; % -------- tikz ----------
    ;; % \\usepackage{tikz}
    ;; % \\usetikzlibrary{external}
    ;; % \\tikzexternalize
    ;;
    ;; % -------- Solve minted + tikz double external processing
    ;; \\tikzset{
    ;; external/system call={%
    ;; xelatex \\tikzexternalcheckshellescape
    ;; -halt-on-error -interaction=batchmode --shell-escape
    ;; -jobname \"image\" \"texsource\"}}

    ;; default latex packages that are placed in the header, which makes org
    ;; works properly. Change it only if nessary
    ;;
    ;; NOTE: default value for referencing
    ;;
    ;;     (setq org-latex-default-packages-alist
    ;;           '(("AUTO" "inputenc" t ("pdflatex"))
    ;;             ("T1" "fontenc" t ("pdflatex"))
    ;;             ("" "graphicx" t)
    ;;             ("" "longtable" nil)
    ;;             ("" "wrapfig" nil)
    ;;             ("" "rotating" nil)
    ;;             ("normalem" "ulem" t)
    ;;             ("" "amsmath" t)
    ;;             ("" "amssymb" t)
    ;;             ("" "capt-of" nil)
    ;;             ("" "hyperref" nil)
    ;;             ))

    ;; NOTE: The default `inputenc' and `fontenc' packages conflicts with
    ;; `xecjk' and `ctex' if you use `pdflatex' TeX engine. The encoding of the
    ;; input latex files don't need to be set.
    ;;
    ;; (setq org-latex-default-packages-alist
    ;;       '(("" "fixltx2e" nil) ("" "graphicx" t) ("" "longtable" nil)
    ;;         ("" "float" nil) ("" "wrapfig" nil) ("" "rotating" nil)
    ;;         ("normalem" "ulem" t) ("" "amsmath" t) ("" "textcomp" t)
    ;;         ("" "marvosym" t) ("" "wasysym" t) ("" "amssymb" t)
    ;;         ("" "hyperref" nil) "\\tolerance=1000"
    ;;         ;;("" "amsmath" t) ;; this package cause error, no need
    ;;         ))

    ;; User's packages that are inserted to the header. They will be loaded
    ;; after `org-latex-default-packages-alist'.
    ;;
    ;; Backup:
    ;;
    ;; (setq org-latex-packages-alist '(("newfloat" "minted" nil)))
    (setq org-latex-packages-alist
          '(;; Auto language localization
            ("AUTO" "babel" t ("pdflatex" "xelatex" "lualatex"))
            ;; CJK font supports
            ("" "xeCJK" t ("xelatex" "lualatex"))
            ;; For `minted' option of `org-latex-src-block-backend'
            ;; ("" "newfloat" nil) ("" "minted" nil)
            ;; math font
            ("" "unicode-math" t ("xelatex" "lualatex"))
            ))

    ;; Removed packages
    ;; -------- CJK supports ---------------------------
    ;;
    ;; -------- NOTE: I am currently using `xeCJK'
    ;; default font adjustment (`xelatex')
    ;; ("" "fontspec" nil ("xelatex" "lualatex"))
    ;; -------- document layout/structure --------------
    ;; ("" "etex" nil)
    ;; ("" "multicol" nil)
    ;; ("" "multind" nil)
    ;; ("" "titlesec" nil)
    ;; -------- spacing --------------------------------
    ;; ("" "setspace"    nil)
    ;; -------- maths ----------------------------------
    ;; -------- grahpics -------------------------------
    ;; ("" "rotating" t)
    ;; ("" "subfig" t)
    ;; ("" "tikz" nil)
    ;; -------- tables --------------------------------
    ;; ("" "booktabs" t)
    ;; ("" "multirow" t)
    ;; ("" "tabularx" t)
    ;; ("" "warpcol" t)
    ;; -------- code listing --------------------------
    ;; ------------ `org-latex-src-block-backend'
    ;; ------------ NOTE: I am currently using `engraved' option
    ;; ---------------- `listings' option
    ;; ("svgnames, table" "xcolor" t)
    ;; ("" "listings" t)
    ;; ---------------- `minted' option
    ;; ("" "minted" t)
    ;; -------- logos ----------------------------------
    ;; ("" "metalogo" t)
    ;; ("" "mflogo" t) ("" "texnames" t) ;; not very useful
    ;; ---- org export --------------------------------------------------------

    (setq org-export-backends '(ascii beamer html latex man md odt org texinfo)
          org-export-use-babel nil
          org-export-with-sub-superscripts '{})

    ;; User-defined entities used in Org to produce special characters in by
    ;; different export backends
    ;;
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

    ;; -------- HTML export backend --------------------------------------
    (require 'ox-html)

    ;; Use the same processing of previewing latex fragments
    (setq org-html-with-latex org-preview-latex-default-process)

    ;; overwrite functions
    (defun org-html--wrap-latex-environment (contents _ &optional caption label)
      "Wrap CONTENTS string within appropriate environment for equations.
When optional arguments CAPTION and LABEL are given, use them for
caption and \"id\" attribute."
      (format "\n<div%s class=\"equation-container\">\n%s%s\n</div>"
              ;; ID.
              (if (org-string-nw-p label) (format " id=\"%s\"" label) "")
              ;; Contents.
              (format "<span class=\"equation\">\n%s\n</span>" contents)
              ;; Caption.
              ;; HACK: disable added label
              ;; (if (not (org-string-nw-p caption)) ""
              ;;   (format "\n<span class=\"equation-label\">\n%s\n</span>"
              ;;           caption))
              (if (org-string-nw-p caption) ""
                (format "\n<span class=\"equation-label\">\n%s\n</span>"
                        caption))
              ))

    (defun org-html-latex-environment (latex-environment _contents info)
      "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
      (let ((processing-type (plist-get info :with-latex))
            (latex-frag (org-remove-indentation
                         (org-element-property :value latex-environment)))
            (attributes (org-export-read-attribute :attr_html latex-environment))
            (label (org-html--reference latex-environment info t))
            (caption (and (org-html--latex-environment-numbered-p latex-environment)
                          (number-to-string
                           (org-export-get-ordinal
                            latex-environment info nil
                            (lambda (l _)
                              (and (org-html--math-environment-p l)
                                   (org-html--latex-environment-numbered-p l))))))))
        (cond
         ((memq processing-type '(t mathjax))
          (org-html-format-latex
           (if (org-string-nw-p label)
               (replace-regexp-in-string "\\`.*"
                                         (format "\\&\n\\\\label{%s}" label)
                                         latex-frag)
             latex-frag)
           'mathjax info))
         ((assq processing-type org-preview-latex-process-alist)
          (let ((formula-link
                 (org-html-format-latex
                  ;; HACK: This append a * to the name of non-math environments
                  ;; (org-html--unlabel-latex-environment latex-frag)
                  latex-frag
                  processing-type info)))
            (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
              (let ((source (org-export-file-uri (match-string 1 formula-link))))
                (org-html--wrap-latex-environment
                 (org-html--format-image source attributes info)
                 info caption label)))))
         (t (org-html--wrap-latex-environment latex-frag info caption label)))))

    ;; -------- ODT backend ----------------------------------------
    (require 'ox-odt)
    (setq org-odt-data-dir (concat org-directory "/addon/odt/styles"))

    ;; -------- LaTeX export backend --------------------------------------
    (require 'ox-latex)
    (setq org-latex-coding-system 'utf-8-unix)

    ;; ------------ TeX engine to use
    ;;
    ;; NOTE: MUST be an element in ‘org-latex-compilers’ (pdflatex, xelatex,
    ;; lualatex), or the empty quote.
    ;;
    ;; NOTE: It replaces the `%latex' macro.
    ;;
    ;; Can also be set in buffers via ;; #+LATEX_COMPILER.
    (setq org-latex-compiler "xelatex")

    ;; ------------ latex commands to process a LaTeX file to a PDF file.
    ;;
    ;;   Default:
    ;;
    ;;     "latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"
    ;;
    ;;   Backup:
    ;;
    ;;     (setq org-latex-pdf-process
    ;;       '(;; "latexmk -pdf -bibtex -f -silent %b"
    ;;         ;; "latexmk -pdfxe -shell-escape -interaction nonstopmode -output-directory %o %f"
    ;;         "latexmk -norc -pdfxe -silent -shell-escape -interaction=nonstopmode -output-directory=%o %f"
    ;;         ))
    ;;
    ;;   NOTE:
    ;;
    ;;       1. Use `latexmk' to automate the whole process
    ;;       2. Add `--shell-escpe' which is required by some LaTeX packages, such as `minted'
    (setq org-latex-pdf-process
          '("latexmk -norc -f -pdfxe -shell-escape -interaction=nonstopmode -output-directory=%o %f"
            ;; "latexmk -pdfxe -shell-escape -interaction nonstopmode -output-directory %o %f"
            ;; "latexmk -norc -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
            ;; "latexmk -c %f"
            ))

    ;; ------------ latex class
    ;; Default value
    ;; (setq org-latex-default-class "article")

    ;; Default value
    ;; (setq org-latex-classes
    ;;       '(("article" "\\documentclass[11pt]{article}"
    ;;          ("\\section{%s}" . "\\section*{%s}")
    ;;          ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;          ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ;;         ("report" "\\documentclass[11pt]{report}"
    ;;          ("\\part{%s}" . "\\part*{%s}")
    ;;          ("\\chapter{%s}" . "\\chapter*{%s}")
    ;;          ("\\section{%s}" . "\\section*{%s}")
    ;;          ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;          ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ;;         ("book" "\\documentclass[11pt]{book}"
    ;;          ("\\part{%s}" . "\\part*{%s}")
    ;;          ("\\chapter{%s}" . "\\chapter*{%s}")
    ;;          ("\\section{%s}" . "\\section*{%s}")
    ;;          ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;          ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

    ;; Backup:
    ;;     (setq org-latex-classes
    ;;           '(;; article
    ;;             ("article" "\\documentclass[11pt]{article}"
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ;;             ;; report
    ;;             ("report" "\\documentclass[11pt]{report}"
    ;;              ;; ("\\part{%s}" . "\\part*{%s}")
    ;;              ("\\chapter{%s}" . "\\chapter*{%s}")
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ;;             ;; book
    ;;             ("book" "\\documentclass[11pt]{book}"
    ;;              ;; ("\\part{%s}" . "\\part*{%s}")
    ;;              ("\\chapter{%s}" . "\\chapter*{%s}")
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}"    . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ;;             ;; letter
    ;;             ("letter" "\\documentclass[11pt]{letter}"
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ;;             ;; scrartcl
    ;;             ("scrartcl" "\\documentclass{scrartcl}"
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ;;             ;; scrreprt
    ;;             ("scrreprt" "\\documentclass{scrreprt}"
    ;;              ;; ("\\part{%s}" . "\\part*{%s}")
    ;;              ("\\chapter{%s}" . "\\chapter*{%s}")
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ;;             ;; scrbook
    ;;             ("scrbook" "\\documentclass{scrbook}"
    ;;              ("\\part{%s}" . "\\part*{%s}")
    ;;              ("\\chapter{%s}" . "\\chapter*{%s}")
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ;;             ;; beamer
    ;;             ;; ("beamer" "\\documentclass{beamer}"
    ;;             ;;  org-beamer-sectioning)
    ;;             ("beamer" "\\documentclass[presentation,9pt]{beamer}
    ;; [DEFAULT-PACKAGES]
    ;; [PACKAGES]
    ;; [EXTRA]"
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ;;             ;; elegantnote
    ;;             ("elegantnote" "\\documentclass{elegantnote}"
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ;;             ;; elegantpaper
    ;;             ("elegantpaper" "\\documentclass{elegantpaper}"
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ;;             ;; elegantbook
    ;;             ("elegantbook" "\\documentclass{elegantbook}"
    ;;              ("\\part{%s}" . "\\part*{%s}")
    ;;              ("\\chapter{%s}" . "\\chapter*{%s}")
    ;;              ("\\section{%s}" . "\\section*{%s}")
    ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ;;             ))

    (add-list-to-list 'org-latex-classes
                      '(("ctexart" "\\documentclass{ctexart}"
                         ("\\section{%s}" . "\\section*{%s}")
                         ("\\subsection{%s}" . "\\subsection*{%s}")
                         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                         ("\\paragraph{%s}" . "\\paragraph*{%s}")
                         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                        ("ctexrep" "\\documentclass{ctexrep}"
                         ("\\part{%s}" . "\\part*{%s}")
                         ("\\chapter{%s}" . "\\chapter*{%s}")
                         ("\\section{%s}" . "\\section*{%s}")
                         ("\\subsection{%s}" . "\\subsection*{%s}")
                         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                        ("ctexbook" "\\documentclass{ctexbook}"
                         ("\\part{%s}" . "\\part*{%s}")
                         ("\\chapter{%s}" . "\\chapter*{%s}")
                         ("\\section{%s}" . "\\section*{%s}")
                         ("\\subsection{%s}" . "\\subsection*{%s}")
                         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                        ))

    ;; ------------

    ;; Default value
    ;; (setq org-export-default-language "en")

    ;; ------------ latex code blocks
    (setq org-latex-src-block-backend 'engraved) ;; 'minted
    ;; (setq org-latex-listings 'minted) ;; obsolete alias of the new `org-latex-src-block-backend'
    ;; FIXME: fix the bug of current version
    ;; For `minted' option of `org-latex-org-latex-src-block-backend'
    (setq org-latex-minted-langs '((jupyter-python "python")
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
                                     ("frame" "lines")
                                     ;; ("outputdir" "/tmp")
                                     ))

    ;; ------------ latax tables
    (setq org-latex-table-caption-above nil
          org-latex-tables-column-borders t)

    ;; -------- Beamer backend
    ;;   (require 'ox-beamer)

    ;; -------- Bibtex backend

    ;; NOTE: requires `bibtex2html' to be installed on your system
    (require 'ox-bibtex)

    ;; ---- Babel ----------------------------------------------------------------

    (setq org-confirm-babel-evaluate nil) ;; no need to confirm

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

    ;; NOTE: make sure all language supports are loaded (deprecated)
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

    ;; -------- Expend babel variables in evaluated results
    ;;
    ;; REF: https://emacs.stackexchange.com/questions/49092/passing-variable-into-a-org-babel-code-on-export
    ;;
    ;; NOTE: there is a `ob-org.el'
    ;;
    (defun org-babel-execute:org (body params)
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

    ;; ------------- Support Calc commands ----------------------------------
    ;;
    ;; REF: https://emacs.stackexchange.com/questions/2943/calling-the-calc-stack-from-babel-displaying-percentages
    ;; Override org-babel-execute:calc
    (require 'ob-calc)
    (defun org-babel-execute:calc (body params)
      "Execute a block of calc code with Babel."
      (unless (get-buffer "*Calculator*")
        (save-window-excursion (calc) (calc-quit)))
      (let* ((vars (mapcar #'cdr (org-babel--get-vars params)))
             (org--var-syms (mapcar #'car vars))
             (var-names (mapcar #'symbol-name org--var-syms)))
        (mapc
         (lambda (pair)
           (calc-push-list (list (cdr pair)))
           (calc-store-into (car pair)))
         vars)
        (mapc
         (lambda (line)
           (when (> (length line) 0)
             (cond
              ;; simple variable name
              ((member line var-names) (calc-recall (intern line)))
              ;; stack operation
              ((string= "'" (substring line 0 1))
               (let ((f (lookup-key calc-mode-map (substring line 1))))
                 (condition-case nil
                     (funcall f nil)        ;; try calling with one arg
                   (error (funcall f)))))   ;; if failed, call without args
              ;; complex expression
              (t
               (calc-push-list
                (list (let ((res (calc-eval line)))
                        (cond
                         ((numberp res) res)
                         ((math-read-number res) (math-read-number res))
                         ((listp res) (error "Calc error \"%s\" on input \"%s\""
                                             (cadr res) line))
                         (t (replace-regexp-in-string
                             "'" ""
                             (calc-eval
                              (math-evaluate-expr
                               ;; resolve user variables, calc built in
                               ;; variables are handled automatically
                               ;; upstream by calc
                               (mapcar #'org-babel-calc-maybe-resolve-var
                                       ;; parse line into calc objects
                                       (car (math-read-exprs line)))))))))
                      ))))))
         (mapcar #'org-babel-trim
                 (split-string (org-babel-expand-body:calc body params) "[\n\r]"))))
      (save-excursion
        (with-current-buffer (get-buffer "*Calculator*")
          (if (equal (cdr (assoc :result-type params)) 'output)
              (math-format-value (calc-top 1)) ; return formatted value if asked for the output
            (calc-eval (calc-top 1))))))

    ;; -------- FIXME: Add timestamp header arg
    ;; Timestamp on babel-execute results block
    ;; REF: https://emacs.stackexchange.com/questions/16850/timestamp-on-babel-execute-results-block
    (defadvice org-babel-execute-src-block (after org-babel-record-execute-timestamp)
      (let ((code-block-params (nth 2 (org-babel-get-src-block-info)))
            (code-block-name (nth 4 (org-babel-get-src-block-info))))
        (let ((timestamp (cdr (assoc :timestamp code-block-params)))
              (result-params (assoc :result-params code-block-params)))
          (if (and (equal timestamp "t") (> (length code-block-name) 0))
              (save-excursion
                (search-forward-regexp (concat "#\\+RESULTS\\(\\[.*\\]\\)?: "
                                               code-block-name))
                (beginning-of-line)
                (search-forward "RESULTS")
                (kill-line)
                (insert (concat (format-time-string "[%F %r]: ") code-block-name)))
            (if (equal timestamp "t")
                (message (concat "Result timestamping requires a #+NAME: "
                                 "and a ':results output' argument.")))))))

    (ad-activate 'org-babel-execute-src-block)

    ;; Examples:
    ;; #+NAME: test-no-timestamp
    ;; #+BEGIN_SRC shell :results output
    ;; echo "This ones doesn't have the right args for timestamping"
    ;; #+END_SRC
    ;;
    ;; #+RESULTS: test-no-timestamp
    ;; : This ones doesn't have the right args for timestamping
    ;;
    ;; #+NAME: test-timestamp
    ;; #+BEGIN_SRC shell :results output :timestamp t
    ;; echo "This one should have a timestamp. Run me again, I update."
    ;; #+END_SRC
    ;;
    ;; #+RESULTS[2017-10-03 05:19:09 AM]: test-timestamp
    ;; : This one should have a timestamp. Run me again, I update.

    ;; -------- Fix inline image display problem

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

    ;; Reference function
    ;; (defun shk-fix-inline-images ()
    ;;   (when org-inline-image-overlays
    ;;     (org-redisplay-inline-images)))

    ;; For newly-added images inline display
    ;; (add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)
    ;; (add-hook 'before-save-hook 'shk-fix-inline-images)
    ;; (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
    ;; (add-hook 'after-save-hook #'org-redisplay-inline-images)

    ;; -------- Customised background color for inline images

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

    ;; -------- ditaa backend --------
    (setq org-ditaa-eps-jar-path "/opt/DitaaEps/DitaaEps.jar"
          org-ditaa-jar-path "/opt/ditaa/ditaa.jar")

    ;; -------- plantuml backend ---------
    (setq org-plantuml-args '("-headless" "-DRELATIVE_INCLUDE=\".\"")
          org-plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\"")
          org-plantuml-jar-path "/opt/plantuml/plantuml.jar")

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

    ;; FIXME: TAGS file cannot be built
    ;; (require 'org-ctags)
    ;; (setq org-ctags-path-to-ctags "/usr/bin/ctags")
    ;; ;; Defined a spacemacs major mode keys
    ;; (add-hook 'org-mode-hook
    ;;           (lambda ()
    ;;             (define-key org-mode-map "\C-co" 'org-ctags-find-tag-interactive)))
    ;; )

    ;; load library-of-babel
    ;; (xy/load-lob)
    ))

(defun org-extra/pre-init-org-contacts ()
  (spacemacs|use-package-add-hook org-contacts
    :pre-init
    (setq org-contacts-files '("~/org/roam/contacts.org.gpg"))))

(defun org-extra/post-init-org-download ()
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-download-edit-cmd "krita %s"
        org-download-image-org-width 480
        org-download-method 'attach
        ;; NOTE: scrot only works on X11, not wayland
        ;; org-download-screenshot-method "scrot -s %s"

        ;; NOTE: gnome-screenshoot works on X11, not wayland
        ;; org-download-screenshot-method "gnome-screenshot -a -f %s"

        ;; NOTE: on Ubuntu 23.10, grim does not work, nor flameshot which depends on grim.
        ;; "compositor doesn't support wlr-screencopy-unstable-v1"
        ;;
        ;; org-download-screenshot-method "grim %s"

        ;; NOTE: gnome shell screenshotUI cannot set save path
        ;; org-download-screenshot-method "gdbus call --session --dest org.gnome.Shell  --object-path /org/gnome/Shell --method org.gnome.Shell.Eval 'Main.screenshotUI.open();'"

        ;; NOTE: [2024-02-08] fameshot requires your Emacs was compiled with X
        ;; (XWayland) support (--with-gtk) ranther than native wayland support
        ;; (--with-pgtk).
        ;;
        ;; org-download-screenshot-method "env XDG_SESSION_TYPE= QT_QPA_PLATFORM=wayland flameshot gui-d 0 -p %s"
        ))

(defun org-extra/post-init-org-ref ()
  (setq org-ref-open-pdf-function
        (lambda (fpath)
          (start-process "zathura"
                         "*bibtex-zathura*" ;; was "*helm-bibtex-zathura*", changed because helm was removed
                         "/usr/bin/zathura" fpath))))

(defun org-extra/post-init-markdown ()
  (add-hook 'markdown-mode-hook #'toc-org-mode))

(defun org-extra/pre-init-org-roam ()
  (spacemacs|use-package-add-hook org-roam
    :pre-init
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
          '(("f" "fleeting" entry
             (file "templates/fleeting.org")
             :target (file "~/org/roam/note_inbox.org")
             :prepend t
             :empty-lines 1
             )
            ("l" "literature" plain
             (file "templates/literature.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t
             )
            ("p" "permanent" plain
             (file "templates/permanent.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t
             )
            ("r" "reference" plain
             (file "templates/reference.org")
             :target (file "${slug}.org")
             :unnarrowed t
             :jump-to-captured t
             )
            ("g" "glossary" plain
             (file "templates/glossary.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t
             )
            ("h" "hub" plain
             (file "templates/hub.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t
             )
            ("j" "project" plain
             (file "templates/project.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :clock-keep t
             :unnarrowed t
             :jump-to-captured t
             )
            ("s" "software" plain
             (file "templates/software.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t
             )
            ("c" "code" plain
             (file "templates/code.org")
             :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
             :unnarrowed t
             :jump-to-captured t
             )))

    (setq org-roam-capture-ref-templates
          '(("r" "reference" plain
             (file "templates/reference-roam.org")
             :target (file "${slug}.org")
             :immediate-finish t
             :jump-to-captured t
             :empty-lines 1)
            ))

    (setq org-roam-dailies-capture-templates
          '(("m" "mind path" entry
             (file "templates/diary-mindpath.org")
             :target (file+olp "%<%Y-%m-%d>.org" ("Mind path"))
             ;; :hook (xy/org-roam-dailies-create-date)
             :empty-lines 1)
            ;; FIXME: error
            ;; ("m" "mind path" item
            ;;  (file "templates/diary-mindpath.org")
            ;;  :target (file+regexp "%<%Y-%m-%d>.org" "endmindmap")
            ;;  ;; :hook (xy/org-roam-dailies-create-date)
            ;;  :empty-lines 1)
            ("t" "timeline" entry
             (file "templates/diary-timeline.org")
             :target (file+olp "%<%Y-%m-%d>.org" ("Timeline"))
             ;; :hook (xy/org-roam-dailies-create-date)
             :empty-lines 1)
            ("n" "note" entry
             (file "templates/diary-entry.org")
             :target (file+olp "%<%Y-%m-%d>.org" ("Notes"))
             ;; :hook (xy/org-roam-dailies-create-date)
             ;; :jump-to-captured
             :empty-lines 1)
            ))

    ;; add org speed keys
    (setq org-speed-commands
          (cons '("x" . org-roam-extract-subtree) org-speed-commands))
    (setq org-speed-commands
          (cons '("X" . org-roam-refile) org-speed-commands))
    (setq org-speed-commands
          (cons '("d" . org-id-get-create) org-speed-commands))
    (setq org-speed-commands
          (cons '("+" . org-roam-alias-add) org-speed-commands))
    (setq org-speed-commands
          (cons '("r" . org-roam-ref-add) org-speed-commands))

    ;; NOTE: This global minor mode is too heavy.
    ;; Turn it on/off manually in org-mode { M-m m r r }
    (org-roam-db-autosync-mode 1)

    ;; FIXME: error when { M-x org-roam-buffer-toggle RET }
    ;; REF: https://github.com/org-roam/org-roam/issues/1732
    ;; (advice-add #'org-roam-fontify-like-in-org-mode
    ;;             :around (lambda (fn &rest args) (save-excursion (apply fn args))))

    ;; Improve org-roam buffer fontification speed
    ;;
    ;; REF: https://github.com/org-roam/org-roam/pull/2340
    ;;
    ;; NOTE: When there are a lot of nodes to write on the org-roam-buffer,
    ;; Emacs starts to hang. This improvement decreases the fontification run
    ;; time from 1.6 sec to 0.07 sec for a 10 node example (in my org-mode
    ;; setup).
    ;;
    ;; The basic idea is that (org-mode) has lots of initial needs, and users
    ;; can have special org-mode hooks. Instead of calling org-mode for every
    ;; node fontification, we could initialize it once and use it later.

    (setq org-roam-fontify-buffer (get-buffer-create "*org-roam-fontify-buffer*"))
    (with-current-buffer org-roam-fontify-buffer (org-mode))
    (defun org-roam-fontify-like-in-org-mode (s)
      "Fontify string S like in Org mode.
Like `org-fontify-like-in-org-mode', but supports `org-ref'."
      ;; NOTE: pretend that the temporary buffer created by `org-fontify-like-in-org-mode' to
      ;; fontify a `cite:' reference has been hacked by org-ref, whatever that means;
      ;;
      ;; `org-ref-cite-link-face-fn', which is used to supply a face for `cite:' links, calls
      ;; `hack-dir-local-variables' rationalizing that `bibtex-completion' would throw some warnings
      ;; otherwise.  This doesn't seem to be the case and calling this function just before
      ;; `org-font-lock-ensure' (alias of `font-lock-ensure') actually instead of fixing the alleged
      ;; warnings messes the things so badly that `font-lock-ensure' crashes with error and doesn't let
      ;; org-roam to proceed further. I don't know what's happening there exactly but disabling this hackery
      ;; fixes the crashing.  Fortunately, org-ref provides the `org-ref-buffer-hacked' switch, which we use
      ;; here to make it believe that the buffer was hacked.
      ;;
      ;; This is a workaround for `cite:' links and does not have any effect on other ref types.
      ;;
      ;; `org-ref-buffer-hacked' is a buffer-local variable, therefore we inline
      ;; `org-fontify-like-in-org-mode' here
      (with-current-buffer org-roam-fontify-buffer
        (erase-buffer)
        (insert s)
        (let ((org-ref-buffer-hacked t))
          (setq-local org-fold-core-style 'overlays)
          (font-lock-ensure)
          (buffer-string))))
    ;; ;; Advice for runtime test
    ;; (defun xy/org-roam-buffer-render-time (orig-fun &rest args)
    ;;   (let ((start-time (float-time)))
    ;;     (apply orig-fun args)
    ;;     (message "The *org-roam* buffer was renderred in %.6f seconds."
    ;;              (- (float-time) start-time))))
    ;; ;; Measure runtime
    ;; (advice-add 'org-roam-backlinks-section :around #'xy/org-roam-buffer-render-time)
    ))

(defun org-extra/post-init-org-roam-ui ()
  ;; (advice-add 'org-roam-ui-mode :before 'org-roam-db-sync)
  (spacemacs|diminish org-roam-ui-mode " ⓤ" " u")
  (spacemacs|diminish org-roam-ui-follow-mode))

(defun org-extra/post-init-alert ()
  (setq alert-default-style 'libnotify))

(defun org-extra/post-init-org-wild-notifier ()
  (setq org-wild-notifier-alert-time '(25 15 10 5 3 1)
        org-wild-notifier-keyword-blacklist '("DONE" "CANCELLED" "MARK" "USELESS")
        org-wild-notifier-keyword-whitelist nil
        org-wild-notifier-tags-blacklist '("ARCHIVE")))

(defun org-extra/init-djvu ()
  (use-package djvu))

;; load org-noter
(defun org-extra/init-org-noter ()
  (use-package org-noter
    :defer t
    :config
    (setq org-noter-auto-save-last-location t
          org-noter-notes-search-path '("~/doc")
          org-noter-always-create-frame t
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

    (setq org-web-tools-archive-wget-options
          '("--ignore-tags=script,iframe"
            "--reject=eot,ttf,otf,*.woff*"
            "--execute" "robots=off"
            "--adjust-extension"
            "--span-hosts"
            "--convert-links"
            "--page-requisites"
            "--timestamping"
            "--no-directories"
            "--user-agent=firefox")
          org-web-tools-archive-wget-html-only-options
          '("--execute" "robots=off"
            "--adjust-extension"
            "--timestamping"
            "--no-directories"
            "--user-agent=firefox"
            ))

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

    ;; FIXME:
    ;; it is strange that `ob-async' requires a reload to work
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
    ;;   - https://github.com/jwiegley/emacs-async/issues/52
    ;;   - https://www.reddit.com/r/emacs/comments/v2p4q9/orgbabel_problems_with_obasync/
    ;;   - https://github.com/astahlman/ob-async/pull/93
    ;; (defun no-hide-overlays (orig-fun &rest args)
    ;;   (setq org-babel-hide-result-overlays nil))

    ;; (advice-add 'ob-async-org-babel-execute-src-block
    ;;             :before #'no-hide-overlays)
    ))

;; For the new option `engraved' of `org-latex-src-block-backend'
(defun org-extra/init-engrave-faces ()
  (use-package engrave-faces
    :defer t))

(defun org-extra/init-org-ql ()
  (use-package org-ql
    :defer t))

(defun org-extra/init-embark-org-roam ()
  (use-package embark-org-roam
    :after (embark org-roam)))

(defun org-extra/init-org-timeblock ()
  (use-package org-timeblock
    :defer t))

(defun org-extra/init-literate-calc-mode ()
  (use-package literate-calc-mode
    :defer t
    ;; :hook
    ;; ((org-mode markdown-mode) . literate-calc-minor-mode)
    ))

;; load org-node
;; (defun org-extra/init-org-node ()
;;   (use-package org-node
;;     :defer t
;;     ;; :after (org org-roam)
;;     ;; :hook (org-mode . org-node-cache-mode)
;;     ;; :config
;;     ;; (org-node-cache-mode 1)
;;     ))

;; reconfigure `org-appear'
;; (defun org-extra/pre-init-org-appear ()
;;   (spacemacs|use-package-add-hook org-appear
;;     :post-config
;;     (setq org-appear-autoentities t
;;           org-appear-autolinks 'just-brackets
;;           org-appear-autosubmarkers t
;;           org-appear-delay 0.8
;;           org-appear-inside-latex t)
;;     ))

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
