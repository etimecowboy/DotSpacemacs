;;; config.el --- Org configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar org-enable-bootstrap-support nil
  "If non-nil Twitter Bootstrap related packages are configured.")

(defvar org-enable-github-support nil
  "If non-nil Github related packages are configured.")

(defvar org-enable-reveal-js-support nil
  "If non-nil, enable export to reveal.js.")

(defvar org-projectile-file "TODOs.org"
  "The file to store project TODOs in. If this is a relative
path, one file per project is used (and the path is relative to
the project root). If it an absolute path, one global file is
used.")

(spacemacs|defvar-company-backends org-mode)

(with-eval-after-load 'org (org-postload))

(defun org-postload()
  "Settings of `org' after it's been loaded."

  ;;;;;;;; org files
  ;; when the file does not exist
  ;; (unless (file-exists-p org-default-notes-file)
  ;;   (shell-command (concat "touch " org-default-notes-file)))
  (setq org-directory (concat my-emacs-workspace "/org")
        org-default-notes-file (concat org-directory "/gtd/Note.org")
        org-combined-agenda-icalendar-file (concat org-directory "/org.ics")
        org-id-locations-file (concat org-directory "/org-id-locations")
        org-clock-persist-file (concat org-directory "/org-clock-save"))

  ;;;;;;;;; load modules
  (setq org-modules
        '(;;;; org official lisps
          org-bbdb org-bibtex org-crypt org-docview
          org-habit org-id org-info org-man org-protocol
          org-w3m org-protocol ;; org-gnus
          ;;;;; org contribute lisps
          ;; NOTE: it is better to load it with `try-require' to avoid
          ;; problem when user don't have `org-plus-contrib' package
          ;; org-bookmark org-mew org-expiry org-git-link
          ))

  ;;;;;;;;; general settings
  ;;;; org completion system
  ;; (setq org-completion-use-iswitchb t)
  ;; (setq org-completion-use-ido t)
  ;;;; Disable ENT to follow links
  ;; (setq org-return-follows-link nil)
  ;;;; Disable inline image display
  ;; (if window-system
  ;;     (setq org-startup-with-inline-images t)
  ;;   (setq org-startup-with-inline-images nil))
  ;; (setq org-startup-with-inline-images nil)
  ;;;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images 'append)

  ;;;; org source folding
  ;; (setq org-startup-folded nil)
  ;; (setq org-cycle-include-plain-lists t)

  ;;;; invisible region
  ;; Check before inserting or deleting a character
  ;; (setq org-catch-invisible-edits 'smart)

  ;;;;;;;; org GTD system
  ;;;; todo items
  (setq org-todo-keywords
        '(;; for tasks
          (sequence "TODO(t)" "SOMEDAY(x)" "NEXT(n)" "STARTED(s!)"
                    "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
          ;; for notes
          (sequence "NEW(a)" "REVIEW(r)" "|" "MARK(m!)" "USELESS(u!)")
          ))
  (setq org-use-fast-todo-selection t
        org-treat-insert-todo-heading-as-state-change t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t)

  ;;;; todo hooks
  ;; todo entry automatically changes to DONE when all children are done
  ;; (defun org-summary-todo (n-done n-not-done)
  ;;   "Switch entry to DONE when all subentries are done, to TODO
  ;; otherwise."
  ;;   (let (org-log-done org-log-states)   ; turn off logging
  ;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  ;; (add-hook ('org-after-todo-statistics-hook 'org-summary-todo)

  ;; - REF: [[http://thread.gmane.org/gmane.emacs.orgmode/21402/focus=21413][orgmode mail-list]]
  ;; - BUG: `state' variable is not recognised
  ;; - TODO: waiting for a `after-schedule-hook' in future release.
  (add-hook 'org-after-todo-state-change-hook
            '(lambda ()
               ;; Delete scheduled time after changing the state to SOMEDAY
               (if (string= org-state "SOMEDAY")
                   (org-remove-timestamp-with-keyword
                    org-scheduled-string))
               ;; Automatically schedule the task to today after enter NEXT
               (if (string= org-state "NEXT")
                   (org-schedule nil "+0"))
               ;; TODO: [spacemacs] Popup notification when done
               ;; (when window-system
               ;;   (when (try-require 'todochiku)
               ;;     (if (string= org-state "DONE")
               ;;         (todochiku-message "Emacs Org"
               ;;                            "Task DONE, Great Work!"
               ;;                            (todochiku-icon 'check)))))
               ))

  ;;;; tags
  (setq org-stuck-projects '("+prj/-SOMEDAY-DONE" ("NEXT" "STARTED"))
        org-use-tag-inheritance t
        org-tags-exclude-from-inheritance '("prj" "crypt" "book"))

  ;;;; tag change with state change
  ;; (setq org-todo-state-tags-triggers
  ;;         '(("TODO"      ("new"))
  ;;           ("NEXT"      ("new"))
  ;;           ("STARTED"   ("new"))
  ;;           ("DONE"      ("new") ("old" . t))
  ;;           ("WAITING"   ("new"))
  ;;           ("SOMEDAY"   ("new"))
  ;;           ("CANCELLED" ("new") ("important") ("old" . t))))

  ;;;; global tags
  ;; - NOTE: define in org files
  ;; (setq org-tag-persistent-alist
  ;;       '(;; GTD contexts
  ;;         (:startgroup)
  ;;         ("@campus" . ?C) ("@BRL" . ?B) ("@library" . ?L)
  ;;         ("@home" . ?H) ("@street" . ?S)
  ;;         (:endgroup)
  ;;         ("@online" . ?O) ("@post" . ?M) ("@email" . ?E)
  ;;         ("@phone" . ?F) ("@people" . ?Z)
  ;;         ;; special tags
  ;;         ("appt" . ?A) ("prj" . ?P) ("repeat" . ?R)
  ;;         ("delegated" . ?D) ("noexport" . ?N)))

  ;;;; properties
  ;; Don't inheritant property for sub-items, since it slows
  ;; down property searchings.
  (setq org-use-property-inheritance nil)

  ;;;; additional properties
  ;; - NOTE: a task should not takes more than 2 hours, otherwise it
  ;; MUST be a project and can be broken into smaller tasks.
  (setq org-global-properties
        '(("Effort_ALL" .
           "0:10 0:20 0:30 1:00 1:30 2:00 2:30 3:00 4:00")
          ("Importance_ALL" .
           "A B C")
          ("SCORE_ALL" .
           "0 1 2 3 4 5 6 7 8 9 10")))

  ;;;; priority
  (setq org-enable-priority-commands           t
        org-highest-priority                  ?A
        org-lowest-priority                   ?C
        org-default-priority                  ?B
        org-priority-start-cycle-with-default  t)

  ;;;; column view
  (setq org-columns-default-format ; Set default column view headings
        "%CATEGORY(Cat.) %PRIORITY(Pri.) %Importance(Imp.) %6TODO(State) %35ITEM(Details) %ALLTAGS(Tags) %5Effort(Plan){:} %6CLOCKSUM(Clock){Total} %SCORE(SCORE)")

  ;;;; clocking
  (setq org-clock-history-length 10
        org-clock-idle-time      15
        org-clock-in-resume      t
        org-clock-into-drawer    t
        org-clock-in-switch-to-state    "STARTED"
        org-clock-out-switch-to-state   "WAITING"
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done  t
        org-clock-persist        t
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-report-include-clocking-task t
        org-clock-persist-query-save t
        org-clock-sound t)
  (org-clock-persistence-insinuate)

  ;;;; time logging
  (setq org-log-done            'time
        org-log-done-with-time  t
        org-log-into-drawer     t
        org-log-redeadline      'note
        org-log-reschedule      'time
        org-log-refile          'time
        org-log-state-notes-insert-after-drawers t
        org-agenda-log-mode-items '(closed state))

  ;;;; notification
  (setq org-agenda-include-diary t) ;; add events in emacs diary

  ;; FIXME: error when activate
  ;; ;;;; update appt each time agenda opened
  ;; (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
  ;; (defadvice  org-agenda-redo (after org-agenda-redo-add-appts)
  ;;   "Pressing `r' on the agenda will also add appointments."
  ;;   (progn
  ;;     (setq appt-time-msg-list nil)
  ;;     (org-agenda-to-appt)))
  ;; (ad-activate 'org-agenda-redo)

  ;;;; todochiku
  ;; - NOTE: already in `appt' setting
  ;; (when window-system
  ;;   (when (try-require 'todochiku)
  ;;     (setq org-show-notification-handler
  ;;           '(lambda (notification)
  ;;              (todochiku-message "org-mode notification" notification
  ;;                                 (todochiku-icon 'emacs))))))

  ;;;;;;;; org capture
  (setq org-capture-templates
        '(("t" "Capture a New Task from Emacs"
           entry (file+headline "~/emacs/org/gtd/Gtd.org" "Task Inbox")
           "** TODO %^{Task} %^G
:LOGBOOK:
- Initial State           \"TODO\"       %U
- Link %a
:END:"
           :empty-lines 1 :prepend t :clock-keep t)

          ("n" "Take a Note from Emacs"
           entry (file+headline "~/emacs/org/gtd/Note.org" "Note Inbox")
           "** NEW %^{Title} %^G
:LOGBOOK:
- Timestamp               \"NEW\"        %U
- Link %a
:END:"
           :empty-lines 1 :prepend t :clock-keep t)

          ("e" "English language study: phrases/sentences"
           entry (file+headline "~/emacs/org/gtd/English.org" "English Inbox")
           "** %? %^g
:LOGBOOK:
- Timestamp                              %U
- Link %a
:END:"
           :empty-lines 1 :prepend t :clock-keep t)

          ("c" "Contacts"
           entry (file+headline "~/emacs/org/gtd/Contacts.org" "New Contacts")
           "** %(org-contacts-template-name)
:PROPERTIES:%(org-contacts-template-email)
:OCCUPATION:
:WEBPAGE:
:PHONE:
:MOBILE:
:QQ/IRC:
:SKYPE:
:ADDRESS:
:NOTE:
:END:"
           :empty-lines 1 :prepend t :clock-keep t)

          ("1" "Capture a New Task from Web Browser"
           entry (file+headline "~/emacs/org/gtd/Gtd.org" "Task Inbox")
           "** TODO %^{Task} %^G
:PROPERTIES:
:DESCRIPTION: %?
:END:
:LOGBOOK:
- Initial State           \"TODO\"       %U
- Link %a
:END:"
           :empty-lines 1 :prepend t :clock-keep t)

          ("2" "Take a Note from Web Browser"
           entry (file+headline "~/emacs/org/gtd/Note.org" "Note Inbox")
           "** NEW %^{Title} %^G
:LOGBOOK:
- Timestamp               \"NEW\"        %U
- Link %a
:END:"
           :empty-lines 1 :prepend t :clock-keep t)

          ("4" "Add a bookmark"
           entry (file+headline "~/emacs/org/gtd/Bookmark.org" "Bookmark Inbox")
           "** NEW %A %^G
:PROPERTIES:
:SCORE: %?
:DESCRIPTION:
:END:
:LOGBOOK:
- Timestamp               \"NEW\"        %U
:END:"
           :empty-lines 1 :prepend t :clock-keep t)
          ))

  ;;;;;;;; org refile

  ;; Targets include this file and any file contributing to the agenda
  ;; up to 3 levels deep
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3)))

  ;; Put the newest item on the top
  (setq org-reverse-note-order t)

  ;; Use paths for refile targets
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps t)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Infomation saved in archives
  (setq org-archive-save-context-info
        '(time file category todo priority itags olpath ltags))

  ;; makes it possible to archive tasks that are not marked DONE
  (setq org-archive-mark-done nil)

  ;;;;;;;; org agenda
  (setq org-agenda-files '("~/emacs/org/gtd/Gtd.org"
                           "~/emacs/org/gtd/Life.org"
                           "~/emacs/org/gtd/Geek.org"
                           "~/emacs/org/gtd/Bookmark.org"
                           "~/emacs/org/gtd/Note.org"
                           "~/emacs/org/gtd/English.org"))
  (setq org-agenda-dim-blocked-tasks nil
        org-agenda-window-frame-fractions '(0.20 . 0.80)
        org-agenda-restore-windows-after-quit t
        org-agenda-window-setup  'current-window
        org-indirect-buffer-display 'current-window
        org-agenda-span 'week
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines nil
        org-agenda-todo-ignore-timestamp nil
        org-agenda-todo-ignore-with-date nil
        ;; Show all items when do a tag-todo search (C-c a M)
        ;; org-agenda-tags-todo-honor-ignore-options nil
        org-agenda-todo-list-sublevels nil
        org-agenda-include-deadlines t
        org-agenda-block-separator "========================================"
        org-agenda-use-time-grid t
        org-agenda-time-grid '((daily today require-timed) "----------------"
                               (800 1000 1200 1400 1600 1800 20:00 22:00))
        org-agenda-sorting-strategy
        '((agenda time-up category-keep priority-down todo-state-up)
        (todo time-up category-keep priority-down todo-state-up)
        (tags time-up category-keep priority-down todo-state-up)
        (search time-up category-keep priority-down todo-state-up)))

  ;;;;;;;; Custom agenda commands
  (setq org-agenda-custom-commands
      '(
        ("d" "Day Planner"
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
                   (org-agenda-timeline-show-empty-dates nil)))

          (tags-todo "TODO<>\"TODO\"+TODO<>\"SOMEDAY\"-SCHEDULED<=\"<+7d>\"-SCHEDULED>\"<+14d>\"-DEADLINE<=\"<+7d>\"-DEADLINE>\"<+14d>\"-repeat-bookmark-appt-note-en-prj"
                     ((org-agenda-overriding-header
                       "Pending Next Actions")
                      (org-tags-match-list-sublevels t)))

          (tags-todo "TODO=\"TODO\"-repeat"
                     ((org-agenda-overriding-header
                       "Task Inbox")
                      (org-tags-match-list-sublevels t)))

          (tags-todo "SCHEDULED>=\"<+1d>\"+SCHEDULED<=\"<+7d>\"-repeat-note-bookmark-en"
                     ((org-agenda-overriding-header
                       "Scheduled Tasks in 7 Days")
                      (org-tags-match-list-sublevels nil)))

          (tags-todo "TODO=\"SOMEDAY\"-sub"
                     ((org-agenda-overriding-header
                       "Future Work")
                      (org-tags-match-list-sublevels nil)))))

        ;;----------------------------------------------------------
        ("w" "Weekly Review"
         ((agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-ndays 7)
                   (org-agenda-deadline-warning-days 30)
                   (org-agenda-use-time-grid nil)
                   (org-agenda-skip-scheduled-if-done nil)
                   (org-agenda-skip-deadline-if-done nil)
                   (org-agenda-skip-timestamp-if-done nil)
                   (org-agenda-skip-archived-trees t)
                   (org-agenda-skip-comment-trees t)
                   (org-agenda-todo-list-sublevel t)
                   (org-agenda-timeline-show-empty-dates t)))

          (tags     "CLOSED<\"<tomorrow>\"-repeat-sub-bookmark-note-idea-ARCHIVE"
                    ((org-agenda-overriding-header
                      "Archieve Closed Next Actions")
                     (org-tags-match-list-sublevels t)
                     (org-agenda-skip-scheduled-if-done nil)
                     (org-agenda-skip-deadline-if-done nil)
                     (org-agenda-skip-timestamp-if-done nil)
                     (org-agenda-skip-archived-trees nil)))

          (tags      "+note+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-7d>\""
                     ((org-agenda-overriding-header
                       "Review and Refile Notes in this week")
                      (org-tags-match-list-sublevels nil)))

          (tags      "+bookmark+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-7d>\""
                     ((org-agenda-overriding-header
                       "Review and Refile bookmarks in this week")
                      (org-tags-match-list-sublevels nil)))

          (tags-todo "TODO<>\"TODO\"+TODO<>\"SOMEDAY\"-repeat-sub-bookmark-appt-note"
                     ;;"TODO<>\"TODO\"+SCHEDULED<\"<tomorrow>\"+SCHEDULED>=\"<-1w>\"-repeat-prj"
                     ((org-agenda-overriding-header
                       "Process Pending Next Actions")
                      (org-tags-match-list-sublevels t)))

          (tags-todo "TODO=\"TODO\"-repeat"
                     ((org-agenda-overriding-header
                      "Schedule Tasks")
                      (org-tags-match-list-sublevels t)))

          (tags      "prj/!-TODO-SOMEDAY-sub"
                     ((org-agenda-overriding-header
                       "Projects Review")
                      (org-tags-match-list-sublevels t)))))

        ;;----------------------------------------------------------
        ("n" "Notes and in the Past 30 days" tags
         "+note+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-30d>\""
         ((org-agenda-overriding-header
           "Recent notes (30d)")
          (org-tags-match-list-sublevels nil)))

        ;;----------------------------------------------------------
        ("b" "Bookmarks in the past 30 days" tags
         "+bookmark+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-30d>\""
         ((org-agenda-overriding-header
           "Recent bookmarks (30d)")
          (org-tags-match-list-sublevels nil)))

        ;;----------------------------------------------------------
        ("l" "English study (30d)" tags
         "+en+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-30d>\""
         ((org-agenda-overriding-header "English study")
          (org-tags-match-list-sublevels nil)))
        ))

  ;;;; Agenda view export C-x C-w
  (setq org-agenda-exporter-settings '((ps-number-of-columns 2)
                                       (ps-landscape-mode t)
                                       ;; (org-agenda-prefix-format " [ ] ")
                                       ;; (org-agenda-with-colors nil)
                                       ;; (org-agenda-remove-tags t)
                                       (org-agenda-add-entry-text-maxlines 5)
                                       (htmlize-output-type 'css)))

;; ;;;;;;;; MobileOrg
;; ;; - NOTE: I use Dropbox serveice
;; (setq org-mobile-directory "~/Dropbox/MobileOrg")
;; (setq org-mobile-encryption-tempfile
;;       (concat org-directory "/orgtmpcrypt"))
;; (unless (file-exists-p org-mobile-encryption-tempfile)
;;   (shell-command (concat "touch " org-mobile-encryption-tempfile)))
;; (setq org-mobile-files org-agenda-files)
;; (setq org-mobile-inbox-for-pull
;;       (concat org-directory "/gtd/from-mobile.org"))
;; (unless (file-exists-p org-mobile-inbox-for-pull)
;;   (shell-command (concat "touch " org-mobile-inbox-for-pull)))

  ;;;;;;;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
        '((C . t) (R . t) ;; (asymptote . t)
          (ditaa . t) (dot . t) (plantuml . t)
          (emacs-lisp . t)
          (latex . t) (org . t)
          (matlab . t) (octave . t)
          (python . t) (perl . t) (ruby . t)
          (shell . t) (gnuplot . t)
          (ein . t) ;; (ipython . t)
          (clojure . t)
          ))

  ;; ensure this variable is defined
  (unless (boundp 'org-babel-default-header-args:sh)
    (setq org-babel-default-header-args:sh '()))

  ;; add a default shebang header argument
  (add-to-list 'org-babel-default-header-args:sh
               '(:shebang . "#!/bin/bash"))

  ;;;; matlab-shell
  ;; (Windows
  ;;  (setq org-babel-matlab-shell-command "C:/MATLAB/R2014a/bin/win64/matlabshell.exe")
  ;;  (Laptop
  ;;   (setq org-babel-matlab-shell-command "matlabshell.exe"))
  ;;  (OfficePC
  ;;   (setq org-babel-matlab-shell-command "C:/PROGRA~1/MATLAB/R2014a/bin/win64/matlabshell.exe")))

  ;; (GNULinux
  ;;  (setq org-babel-matlab-shell-command "MATLAB"))

  (setq org-babel-default-header-args:matlab
  '((:results . "output") (:session . "*MATLAB*")))

  ;;;;;;;; src block
  (setq org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-export-babel-evaluate nil
        org-src-tab-acts-natively t
        org-edit-src-turn-on-auto-save t
        org-adapt-indentation nil ;; it is not good for my snippets
        org-edit-src-content-indentation 2
        org-enable-fixed-width-editor t
        org-special-ctrl-o t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window
        org-src-ask-before-returning-to-edit-buffer nil)

  ;; ;; Delete babel temporary diretory instead of trash it.
  ;; ;; macro `bypass-trash-in-function(fun)' sets FUN to always use
  ;; ;; normal deletion, and never trash.
  ;; (mapc (lambda (fun) (eval `(bypass-trash-in-function ,fun)))
  ;;       '(org-babel-remove-temporary-directory))

  ;;;;;;;; Org export
  (setq org-export-backends
        '(ascii beamer html latex md org odt freemind koma-letter))
  (setq org-file-apps ;; set default viewer for exported files
        '((auto-mode       . emacsclient)
          ("\\.odc\\'"     . system)
          ("\\.odf\\'"     . system)
          ("\\.odi\\'"     . system)
          ("\\.otp\\'"     . system)
          ("\\.odp\\'"     . system)
          ("\\.otg\\'"     . system)
          ("\\.odg\\'"     . system)
          ("\\.ots\\'"     . system)
          ("\\.ods\\'"     . system)
          ("\\.odm\\'"     . system)
          ("\\.ott\\'"     . system)
          ("\\.odt\\'"     . system)
          ("\\.mm\\'"      . system)
          ("\\.pdf\\'"     . system)
          ("\\.x?html?\\'" . system)
          ("\\.png\\'"     . system)
          ("\\.jpg\\'"     . system)
          ("\\.bmp\\'"     . system)
          ("\\.gif\\'"     . system)
          ))
  ;; (GNULinux
  (setq org-file-apps ;; set default viewer for exported files
        '((auto-mode       . emacsclient)
          ("\\.x?html?\\'" . system)
          ;; use mupdf for normal viewing
          ("\\.pdf\\'"     . "mupdf -b 8 -r 96 %s")
          ;; use evice for viewing specific number of page
          ;; ("\\.pdf::\\(\\d+\\)\\'" . "evince -p %1 %s")
          ("\\.pdf::\\(\\d+\\)\\'" . "okular -p %1 %s")
          ("\\.pdf.xoj"    . "xournal %s")
          ("\\.png\\'"     . "display %s")
          ("\\.jpg\\'"     . "display %s")
          ("\\.bmp\\'"     . "display %s")
          ("\\.gif\\'"     . "display %s")
          ))
  ;; )

  ;; FIXME: there is a problem with history files, cannot start new emacs process for exporting
  ;; (setq org-export-in-background t) ;; use background export by default

  ;;;; Special symboles
  ;; check http://orgmode.org/manual/Special-symbols.html
  ;; Just use standard \nbsp{} instead of custom entities
  ;; (setq org-entities-user
  ;;       '(("sp" "~" nil "&nbsp;" " " " " " ") ;; non-breaking spaces
  ;;         ))

  ;;;;;;;; LaTeX export
  ;; (require 'ox)
  ;; (require 'ox-latex)
  ;; (require 'ox-beamer)
  (setq org-latex-coding-system 'utf-8-unix)
  (setq org-latex-table-caption-above nil)
  (setq org-latex-tables-column-borders t)
  ;; code listing settings, new `minted' is also supported
  (setq org-latex-listings t)
  (setq org-latex-preview-ltxpng-directory
        "./") ;; fix the bug of current version

  ;; NOTE: Use org to write the draft of the document, and you can
  ;; fine-tuning of the latex template for the final version.
  (setq org-latex-classes
   (quote (("beamer"
            "\\documentclass[presentation,9pt]{beamer}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
           ;;-----------------------------------------------
           ("article" "\\documentclass[11pt]{article}"
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
           ;;------------------------------------------------
           ("report" "\\documentclass[11pt]{report}"
            ;; ("\\part{%s}" . "\\part*{%s}")
            ("\\chapter{%s}" . "\\chapter*{%s}")
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
           ;;-------------------------------------------------
           ("book" "\\documentclass[11pt]{book}"
            ;; ("\\part{%s}" . "\\part*{%s}")
            ("\\chapter{%s}" . "\\chapter*{%s}")
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
           ;;--------------------------------------------------
           ("letter" "\\documentclass[11pt]{letter}"
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
           ;;--------------------------------------------------
           ("scrartcl" "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
           ;;--------------------------------------------------
           ("scrreprt" "\\documentclass{scrreprt}"
             ;; ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
           ;;--------------------------------------------------
           ("scrbook" "\\documentclass{scrbook}"
             ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
           ;;--------------------------------------------------
           ;;--------------------------------------------------
           ;; ("beamer" "\\documentclass{beamer}"
           ;;  org-beamer-sectioning)
           ;;--------------------------------------------------
           ;; NOTE: ctex documentclasses, no need to use ctex package
           ("ctexart" "\\documentclass[UTF8, winfonts, cs4size, \
a4paper, cap, punct, nospace, indent, fancyhdr, hypperref, fntef]\
{ctexart}"
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
           ;;--------------------------------------------------
           ("ctexrep" "\\documentclass[UTF8, winfonts, cs4size, \
a4paper, cap, punct, nospace, indent, fancyhdr, hypperref, fntef]\
{ctexrep}"
            ("\\part{%s}" . "\\part*{%s}")
            ("\\chapter{%s}" . "\\chapter*{%s}")
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
           ;;--------------------------------------------------
           ("ctexbook" "\\documentclass[UTF8, winfonts, cs4size, \
a4paper, cap, punct, nospace, indent, fancyhdr, hypperref, fntef]\
{ctexbook}"
            ("\\part{%s}" . "\\part*{%s}")
            ("\\chapter{%s}" . "\\chapter*{%s}")
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
           ;;--------------------------------------------------
           ;; nice layout, but not very useful
           ;; ("pracjourn" "\\documentclass{pracjourn}"
           ;;   ("\\section{%s}" . "\\section*{%s}")
           ;;   ("\\subsection{%s}" . "\\subsection*{%s}")
           ;;   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ;;   ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ;;   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
           )))

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

  ;; NOTE: LaTeX header that will be used when processing a fragment
  (setq org-format-latex-header
"\\documentclass{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\usepackage{tikz}
\\usetikzlibrary{
arrows, calc, fit, patterns, plotmarks, shapes, shadows,
datavisualization, er, automata, backgrounds, chains, topaths,
trees, matrix, fadings, shadings, through, positioning, scopes,
intersections, fixedpointarithmetic, petri,
decorations.pathreplacing, decorations.pathmorphing,
decorations.markings}
\\usepackage{pgfgantt}

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

  (setq org-format-latex-options
      '(:foreground default :background
                    default :scale 1.0
                    :html-foreground
                    "Black"
                    :html-background
                    "Transparent"
                    :html-scale 1.0
                    :matchers ("begin"
                               "$1" "$"
                               "$$"
                               "\\("
                               "\\[")))

  (setq org-format-latex-signal-error t)
  (setq org-latex-create-formula-image-program 'imagemagick)

  ;; Use latexmk instead of xelatex
  (setq org-latex-pdf-process
        '("latexmk -pdf -bibtex -f -silent %b"
          "latexmk -c"))

  

  ;;;;; html export
  ;; 更好的解决方法: html输出时换行带来的多余空格
  ;; REF: (@url :file-name "http://www.newsmth.net/nForum/#!article/Emacs/103680" :display "newsmth.net")
  ;; 这儿有一种临时解决方法[1][2]，通过给函数 org-html-paragraph 添加
  ;; advice，使得导出 html 前自动将段落中的多行中文合并为一行，且不会影响
  ;; 源文件，个人认为还算实用，可供参考：
  ;; REF: (@url :file-name "http://fasheng.github.io/blog/2013-09-25-fix-chinese-space-issue-when-exporting-org-mode-to-html.html" :display "[1]")
  ;; REF: (@url :file-name "https://gist.github.com/fasheng/6696398 " :display "[2]")
  ;; NOTE: add to `org-post-load'
  (defadvice org-html-paragraph (before fsh-org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
  unwanted space when exporting org-mode to html."
    (let ((fixed-contents)
          (orig-contents (ad-get-arg 1))
          (reg-han "[[:multibyte:]]"))
      (setq fixed-contents (replace-regexp-in-string
                            (concat "\\(" reg-han "\\) *\n *\\(" reg-han "\\)")
                            "\\1\\2" orig-contents))
      (ad-set-arg 1 fixed-contents)
      ))

  

;;;;; odt export
  ;; (require 'ox-odt)
  ;; NOTE: fixed already
  ;; (setq org-odt-data-dir
  ;;       (concat org-directory "/addon/odt/styles"))



;; ;;;;; bibtex export
;;   ;; NOTE: require bibtex2html
;;   (GNULinux
;;    (try-require 'ox-bibtex))


;;     ;;;;;;;; `org-crypt'
;;     (require 'org-crypt)
;;     (org-crypt-use-before-save-magic)
;;     (setq org-use-tag-inheritance t   ;; Inherit tags in most of cases
;;           org-tags-exclude-from-inheritance (quote ("crypt" "prj" "book"))
;;           org-crypt-key nil)
;;     ;; (setq auto-save-default nil)
;;     ;; Auto-saving does not cooperate with org-crypt.el: so you need
;;     ;; to turn it off if you plan to use org-crypt.el quite often.
;;     ;; Otherwise, you'll get an (annoying) message each time you
;;     ;; start Org.
;;     ;; To turn it off only locally, you can insert this:
;;     ;;
;;     ;; # -*- buffer-auto-save-file-name: nil; -*-
;;     )
;;   (setq spaceline-org-clock-p t)

    (message "* ---[ org post-load configuration is complete ]---"))
