  ;;; todo hooks
  ;; todo entry automatically changes to DONE when all children are done
  ;; (defun org-summary-todo (n-done n-not-done)
  ;;   "Switch entry to DONE when all subentries are done, to TODO
  ;; otherwise."
  ;;   (let (org-log-done org-log-states)   ; turn off logging
  ;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  ;; (add-hook ('org-after-todo-statistics-hook #'org-summary-todo)
  ;; - REF: http://thread.gmane.org/gmane.emacs.orgmode/21402/focus=21413
  ;; - FIXME: `state' variable is not recognised
  ;; - TODO: waiting for a `after-schedule-hook' in future release.
  (add-hook 'org-after-todo-state-change-hook
            #'(lambda ()
                ;; Delete scheduled time after changing the state to SOMEDAY
                (if (or (string= org-state "SOMEDAY") (string= org-state "TODO"))
                   (org-remove-timestamp-with-keyword org-scheduled-string))
                ;; Automatically schedule the task to today after enter NEXT
                (if (string= org-state "NEXT")
                    (org-schedule nil "+0"))
                (if (string= org-state "DONE")
                    (alert "WELL DONE" :title "Agenda" :category 'Emacs :severity 'trivial))
                (if (string= org-state "CANCELLED")
                    (alert "Task Cancelled" :title "Agenda" :category 'Emacs :severity 'trivial))
                (if (string= org-state "REVIEW")
                    (org-fc-type-double-init))
               ))


  (add-to-list 'org-after-todo-state-change-hook
               '(lambda ()
                  ;; record the start time of a task
                  (when (equal org-state "STARTED")
                   (xy/org-roam-copy-todo-to-today))
                  ;; record the end time of a task
                  (when (equal org-state "DONE")
                    (xy/org-roam-copy-todo-to-today))
                  ))
