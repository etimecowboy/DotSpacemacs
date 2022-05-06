(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(custom-safe-themes
   '("d600c677f1777c1e4bfb066529b5b73c0179d0499dd4ffa3f599a0fb0cfbd501" default))
 '(global-hardhat-mode t)
 '(hardhat-basename-protected-regexps
   '("~\\'" "\\.lock\\'" "\\.ix\\'" "\\`test\\.out\\'" "-autoloads\\.el\\'" "\\`Desktop\\.ini\\'" "\\`META\\.yml\\'" "\\`MYMETA\\.yml\\'" "\\`TAGS\\'" "\\`Thumbs\\.db\\'" "\\`\\.dropbox\\'" "\\`\\.dropbox\\.cache\\'" "\\`\\.emacs\\.desktop\\'" "\\`\\.emacs\\.desktop\\.lock\\'" "\\.orig\\'" "\\.rej\\'" "\\.bak\\'"))
 '(hardhat-fullpath-protected-regexps
   '("~/\\.emacs\\.d/elpa/" "~/\\.cpan/" "~/\\.cabal/" "~/perl5/perlbrew/" "~/\\.npm/" "~/\\.virtualenv/" "~/\\.virthualenv/" "~/\\.rvm/" "/[._]build/" "/\\.bzr/" "/\\.coverage/" "/\\.git/" "/\\.hg/" "/\\.rspec/" "/\\.sass-cache/" "/\\.svn/" "/_MTN/" "/_darcs/" "/CVS/" "/pm_to_blib/" "/RCS/" "/SCCS/" "/blib/" "/test_output/" "~/\\.emacs\\.d/\\.cask/" "~/\\.cask/" "~/\\.cargo/" "~/\\.conda/" "~/\\.docker/" "~/\\.local/" "~/\\.rustup/" "~/\\.ssh/" "~/bin/"))
 '(ispell-program-name "hunspell")
 '(org-after-todo-state-change-hook
   '((lambda nil
       (when
           (equal org-state "STARTED")
         (xy/org-roam-copy-todo-to-today))
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
          (org-schedule nil "+0"))
      (if
          (string= org-state "DONE")
          (alert "WELL DONE" :title "Agenda" :category 'Emacs :severity 'trivial))
      (if
          (string= org-state "CANCELLED")
          (alert "Task Cancelled" :title "Agenda" :category 'Emacs :severity 'trivial))
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
       (tags-todo "TODO=\"TODO\"-SCHEDULED-repeat"
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
 '(org-appear-autolinks 'just-brackets)
 '(org-appear-autosubmarkers t)
 '(org-appear-delay 0.8)
 '(org-appear-inside-latex t)
 '(org-archive-save-context-info '(time file category todo priority itags olpath ltags))
 '(org-attach-archive-delete 'query)
 '(org-attach-id-dir "data/")
 '(org-attach-store-link-p 'attached)
 '(org-capture-templates
   '(("t" "Task" entry
      (file+headline "~/org/roam/inbox.org" "Tasks")
      "** TODO %^{Task}
:LOGBOOK:
- Create time: %U
- From: %a
:END:

Resources: [add existing nodes here!]" :prepend t :empty-lines 1 :clock-keep t)
     ("n" "Note" entry
      (file+headline "~/org/roam/inbox.org" "Notes")
      "** NEW %^{Title}
:LOGBOOK:
- Create time: %U
- From: %a
:END:

Resources: [add existing nodes here!]" :prepend t :empty-lines 1 :clock-keep t)
     ("e" "English" entry
      (file+headline "~/org/roam/inbox.org" "English")
      "** NEW %?
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:
:LOGBOOK:
- Create time: %U
- From: %a
:END:" :prepend t :empty-lines 1 :clock-keep t)
     ("b" "Bookmark" entry
      (file+headline "~/org/roam/inbox.org" "Bookmark")
      "** NEW %a
:PROPERTIES:
:SCORE: %?
:END:
:LOGBOOK:
- Create time: %U
- From: %a
:END:

- URL: %L
- Resources: [add existing nodes here!]
- Description:" :prepend t :empty-lines 1 :clock-keep t)
     ("c" "Contacts" entry
      (file "~/org/roam/contacts.org.gpg")
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
      (file "~/org/roam/passwords.org.gpg")
      "* %?
:LOGBOOK:
- Create time: %U
- From: %a
:END:

- Website:
- Username:
- Password:
- Resources: [add existing nodes here!]
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
 '(org-download-image-org-width 400)
 '(org-download-method 'attach)
 '(org-download-screenshot-method "scrot -s %s")
 '(org-edit-src-turn-on-auto-save t)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii beamer html latex man md odt org texinfo))
 '(org-export-use-babel nil)
 '(org-export-with-sub-superscripts '{})
 '(org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)))
 '(org-global-properties
   '(("NUM_POMODORO_ALL" . "0 1 2 3 4 5")
     ("SCORE_ALL" . "0 1 2 3 4 5")))
 '(org-indirect-buffer-display 'current-window)
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
   '(ol-bbdb ol-bibtex org-crypt ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe org-mouse org-protocol ol-rmail ol-w3m))
 '(org-plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\""))
 '(org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
 '(org-refile-targets '((nil :maxlevel . 4) (org-agenda-files :maxlevel . 4)))
 '(org-refile-use-outline-path 'file)
 '(org-reverse-note-order t)
 '(org-roam-capture-ref-templates
   '(("a" "annote" entry "** %?
:LOGBOOK:
- Create time: %U
:END:

${body}" :target
(file+head "${slug}.org" "#+title: ${title}
#+filetags: ref
* Abstract

* Local backup
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:")
:immediate-finish t :jump-to-captured t :empty-lines 1)))
 '(org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
      :prepend t :empty-lines 1 :unnarrowed t)
     ("p" "project" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+category: ${title}
#+filetags: PROJECT
* Goal

* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:

** TODO Define the goal of the project.
** TODO Add areas-of-responsibility tags.
** TODO Set a project deadline.")
      :prepend t :empty-lines 1 :clock-keep t :unnarrowed t)))
 '(org-roam-completion-everywhere t)
 '(org-roam-dailies-capture-templates
   '(("d" "default" entry "* %U %?" :target
      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:")
      :prepend t :empty-lines 1 :unnarrowed t)))
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
 '(org-tag-persistent-alist '(("PROJECT" . 112)))
 '(org-todo-keywords
   '((sequence "TODO(t)" "SOMEDAY(x)" "NEXT(n)" "STARTED(s!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
     (sequence "NEW(a)" "REVIEW(r!)" "|" "MARK(m!)" "USELESS(u!)")))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-treat-insert-todo-heading-as-state-change t)
 '(org-use-tag-inheritance nil)
 '(org-wild-notifier-alert-time '(25 15 10 5 3 1))
 '(org-wild-notifier-keyword-blacklist '("DONE" "CANCELLED" "MARK" "USELESS"))
 '(org-wild-notifier-keyword-whitelist nil)
 '(org-wild-notifier-tags-blacklist '("ARCHIVE"))
 '(plantuml-indent-level 4)
 '(time-stamp-format " <%Y-%02m-%02d %3a %02H:%02M by %u on %s>")
 '(time-stamp-time-zone t)
 '(valign-fancy-bar t))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
