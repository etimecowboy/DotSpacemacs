(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d600c677f1777c1e4bfb066529b5b73c0179d0499dd4ffa3f599a0fb0cfbd501" default))
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
 '(org-agenda-exporter-settings
   '((ps-number-of-columns 2)
     (ps-landscape-mode t)
     (org-agenda-add-entry-text-maxlines 5)
     (htmlize-output-type 'css)))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
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
 '(org-roam-dailies-capture-templates
   '(("d" "default" entry "* %U %?" :target
      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:")
      :prepend t :empty-lines 1 :unnarrowed t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
