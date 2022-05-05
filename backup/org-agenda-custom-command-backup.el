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

            (tags "CLOSED<\"<tomorrow>\"\
-repeat-sub-bookmark-note-idea-ARCHIVE"
                  ((org-agenda-overriding-header
                    "Archieve Closed Next Actions")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-skip-scheduled-if-done nil)
                   (org-agenda-skip-deadline-if-done nil)
                   (org-agenda-skip-timestamp-if-done nil)
                   (org-agenda-skip-archived-trees nil)))

            (tags "+note+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-7d>\""
                  ((org-agenda-overriding-header
                    "Review and Refile Notes in this week")
                   (org-tags-match-list-sublevels nil)))

            (tags "+bookmark+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-7d>\""
                  ((org-agenda-overriding-header
                    "Review and Refile bookmarks in this week")
                   (org-tags-match-list-sublevels nil)))

            (tags-todo "TODO<>\"TODO\"+TODO<>\"SOMEDAY\"\
-repeat-sub-bookmark-appt-note"
                       ;; "TODO<>\"TODO\"+SCHEDULED<\"<tomorrow>\"\
                       ;; +SCHEDULED>=\"<-1w>\"-repeat-prj"
                       ((org-agenda-overriding-header
                         "Process Pending Next Actions")
                        (org-tags-match-list-sublevels t)))

            (tags-todo "TODO=\"TODO\"-repeat"
                       ((org-agenda-overriding-header
                         "Schedule Tasks")
                        (org-tags-match-list-sublevels t)))

            (tags "PROJECT/!-TODO-SOMEDAY-sub"
                  ((org-agenda-overriding-header
                    "Projects Review")
                   (org-tags-match-list-sublevels t)))))

          ("n" "Notes and in the Past 30 days" tags
           "+note+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-30d>\""
           ((org-agenda-overriding-header
             "Recent notes (30d)")
            (org-tags-match-list-sublevels nil)))

          ("b" "Bookmarks in the past 30 days" tags
           "+bookmark+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-30d>\""
           ((org-agenda-overriding-header
             "Recent bookmarks (30d)")
            (org-tags-match-list-sublevels nil)))

          ("l" "English study (30d)" tags
           "+en+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-30d>\""
           ((org-agenda-overriding-header "English study")
            (org-tags-match-list-sublevels nil)))

;;                 (tags-todo "TODO<>\"TODO\"+TODO<>\"SOMEDAY\"\
;; -SCHEDULED<=\"<+7d>\"-SCHEDULED>\"<+14d>\"-DEADLINE<=\"<+7d>\"-DEADLINE>\"<+14d>\"\
;; -repeat-bookmark-appt-note-en-prj"
;;                            ((org-agenda-overriding-header
;;                              "Pending Next Actions")
;;                             (org-tags-match-list-sublevels t)))
