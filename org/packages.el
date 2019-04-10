;;; packages.el --- org layer packages file for Spacemacs.
;;
;; Copyright (c) 2018-2020 etimecowboy & Contributors
;;
;; Author: etimecowboy <etimecowboy@gmail.com>
;; URL:
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;;
;; My customized org layer that assist coding and documentation.
;; The shipped spacemacs org layer is shadowed by this one.

;;; Code:

(defconst org-packages
  '(
    ;; packages in spacemacs org layer
    ;; TODO Keep useful ones and remove useless ones.
    company
    company-emoji
    emoji-cheat-sheet-plus
    (evil-org :location local)
    evil-surround
    gnuplot
    htmlize
    mu4e
    ;; ob, org and org-agenda are installed by `org-plus-contrib'
    (ob :location built-in)
    (org :location built-in)
    (org-agenda :location built-in)
    org-download
    org-mime
    org-pomodoro
    org-present
    (org-projectile :toggle (configuration-layer/package-usedp 'projectile))
    (ox-twbs :toggle org-enable-bootstrap-support)
    ;; use a for of ox-gfm to fix index generation
    (ox-gfm :location (recipe :fetcher github :repo "syl20bnr/ox-gfm")
            :toggle org-enable-github-support)
    (ox-reveal :toggle org-enable-reveal-js-support)
    persp-mode
    ;; my addon packges
    ob-async
    ob-restclient
    ob-ipython
    ))

(defun org/post-init-company ()
  (spacemacs|add-company-hook org-mode)
  (push 'company-capf company-backends-org-mode))

(defun org/post-init-company-emoji ()
  (push 'company-emoji company-backends-org-mode))

(defun org/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'org-mode-hook 'spacemacs/delay-emoji-cheat-sheet-hook))

(defun org/init-evil-org ()
  (use-package evil-org
    :commands (evil-org-mode evil-org-recompute-clocks)
    :init (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (progn
      (evil-define-key 'normal evil-org-mode-map
        "O" 'evil-open-above)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "C" 'evil-org-recompute-clocks)
      (spacemacs|diminish evil-org-mode " ⓔ" " e"))))

(defun org/post-init-evil-surround ()
  (defun spacemacs/add-org-surrounds ()
    (push '(?: . spacemacs//surround-drawer) evil-surround-pairs-alist)
    (push '(?# . spacemacs//surround-code) evil-surround-pairs-alist))
  (add-hook 'org-mode-hook 'spacemacs/add-org-surrounds))

(defun org/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'org-mode
            "tp" 'org-plot/gnuplot)))

(defun org/init-htmlize ()
  (use-package htmlize
    :defer t))

(defun org/pre-init-mu4e ()
  ;; Load org-mu4e when mu4e is actually loaded
  (spacemacs|use-package-add-hook mu4e
    :post-config (require 'org-mu4e nil 'noerror)))

;; load ob-ipython
(defun org/init-ob-ipython ()
  (use-package ob-ipython
    :defer t
    :after ob))

;; load ob-restclient
(defun org/init-ob-restclient ()
  (use-package ob-restclient
    :defer t
    :after ob))

;; load ob-async
(defun org/init-ob-async ()
  (use-package ob-async
    :defer t
    :after ob
    :config
    (progn
      (setq ob-async-no-async-languages-alist '("ipython")))))

(defun org/init-ob ()
  (use-package ob
    :defer t
    :init
    (progn
      (setq org-babel-load-languages
            '((emacs-lisp . t) (shell . t) (restclient . t)
              (ditaa . t) (dot . t) (plantuml . t) (gnuplot . t)
              (latex . t) (org . t)
              (python . t) (perl . t) (ruby . t)
              (matlab . t) (octave . t)
              (C . t) (R . t)
              ;; FIXME gives an error when ipython and jupyter are not installed
              ;; https://github.com/syl20bnr/spacemacs/issues/9941
              ;; (ipython . t)
              ))

      (defun spacemacs//org-babel-do-load-languages ()
        "Load all the languages declared in `org-babel-load-languages'."
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages))

      (add-hook 'org-mode-hook 'spacemacs//org-babel-do-load-languages)
      ;; Fix redisplay of inline images after a code block evaluation.
      (add-hook 'org-babel-after-execute-hook 'spacemacs/ob-fix-inline-images))
    :config
    (progn
      ;; Use bash as the default shell
      (setq org-babel-shell-names '("bash" "sh" "csh" "dash" "zsh" "fish"))

      ;; ensure this variable is defined
      (unless (boundp 'org-babel-default-header-args:sh)
        (setq org-babel-default-header-args:sh '()))

      ;; add a default shebang header argument
      (add-to-list 'org-babel-default-header-args:sh
                   '(:shebang . "#!/bin/bash"))

      ;; TODO change to shell / python
      (setq org-babel-default-header-args:matlab
            '((:results . "output") (:session . "*MATLAB*")))

      ;; Add timestamp for src blocks
      ;; REF: https://emacs.stackexchange.com/questions/16850/timestamp-on-babel-execute-results-block
      ;; NOTE: you have to define #+NAME: header for the block
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
      )
    ))

(defun org/init-org ()
  (use-package org
    :defer t
    :commands (orgtbl-mode org-clock-persistence-insinuate)
    :init
    (progn
      ;; org files
      ;;;; spacemacs defaults
      ;; (setq org-clock-persist-file (concat spacemacs-cache-directory
      ;;                                      "org-clock-save.el")
      ;;       org-id-locations-file (concat spacemacs-cache-directory
      ;;                                     ".org-id-locations")
      ;;       org-publish-timestamp-directory (concat spacemacs-cache-directory
      ;;                                               ".org-timestamps/")
      ;;;; my defaults
      ;; when the file does not exist
      ;; (unless (file-exists-p org-default-notes-file)
      ;;   (shell-command (concat "touch " org-default-notes-file)))
      (setq org-directory (concat my-emacs-workspace "/org")
            org-default-notes-file (concat org-directory "/gtd/Note.org")
            org-combined-agenda-icalendar-file (concat org-directory "/org.ics")
            org-id-locations-file (concat org-directory "/org-id-locations")
            org-clock-persist-file (concat org-directory "/org-clock-save")
            org-publish-timestamp-directory (concat org-directory
                                                    "/org-timestamps/"))
      ;; spacemacs defulats
      (setq ;; org-log-done t ;; moved to :config
            org-startup-with-inline-images t
            org-image-actual-width nil
            org-src-fontify-natively t
            ;; this is consistent with the value of
            ;; `helm-org-headings-max-depth'.
            org-imenu-depth 8)

      (with-eval-after-load 'org-indent
        (spacemacs|hide-lighter org-indent-mode))
      (let ((dir (configuration-layer/get-layer-local-dir 'org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))
      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      ;; Follow the confirm and abort conventions
      (with-eval-after-load 'org-capture
        (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
          dotspacemacs-major-mode-leader-key 'org-capture-finalize
          "c" 'org-capture-finalize
          "k" 'org-capture-kill
          "a" 'org-capture-kill
          "r" 'org-capture-refile))

      (with-eval-after-load 'org-src
        (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
          dotspacemacs-major-mode-leader-key 'org-edit-src-exit
          "c" 'org-edit-src-exit
          "a" 'org-edit-src-abort
          "k" 'org-edit-src-abort))

      (let ((dir (configuration-layer/get-layer-local-dir 'org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))
      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      ;; Insert key for org-mode and markdown a la C-h k
      ;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
      (defun spacemacs/insert-keybinding-org (key)
        "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
        (interactive "kType key sequence: ")
        (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
          (if (null (equal key "\r"))
              (insert
               (format tag (help-key-description key nil)))
            (insert (format tag ""))
            (forward-char -8))))

      (dolist (prefix '(("me" . "export")
                        ("mx" . "text")
                        ("mh" . "headings")
                        ("mi" . "insert")
                        ("mS" . "subtrees")
                        ("mt" . "tables")
                        ("mtd" . "delete")
                        ("mti" . "insert")
                        ("mtt" . "toggle")))
        (spacemacs/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "'" 'org-edit-special
        "c" 'org-capture
        "d" 'org-deadline
        "D" 'org-insert-drawer
        "ee" 'org-export-dispatch
        "f" 'org-set-effort
        "P" 'org-set-property
        ":" 'org-set-tags

        "a" 'org-agenda
        "b" 'org-tree-to-indirect-buffer
        "A" 'org-archive-subtree
        "l" 'org-open-at-point
        "T" 'org-show-todo-tree

        "." 'org-time-stamp
        "!" 'org-time-stamp-inactive

        ;; headings
        "hi" 'org-insert-heading-after-current
        "hI" 'org-insert-heading
        "hs" 'org-insert-subheading

        ;; More cycling options (timestamps, headlines, items, properties)
        "L" 'org-shiftright
        "H" 'org-shiftleft
        "J" 'org-shiftdown
        "K" 'org-shiftup

        ;; Change between TODO sets
        "C-S-l" 'org-shiftcontrolright
        "C-S-h" 'org-shiftcontrolleft
        "C-S-j" 'org-shiftcontroldown
        "C-S-k" 'org-shiftcontrolup

        ;; Subtree editing
        "Sl" 'org-demote-subtree
        "Sh" 'org-promote-subtree
        "Sj" 'org-move-subtree-down
        "Sk" 'org-move-subtree-up

        ;; tables
        "ta" 'org-table-align
        "tb" 'org-table-blank-field
        "tc" 'org-table-convert
        "tdc" 'org-table-delete-column
        "tdr" 'org-table-kill-row
        "te" 'org-table-eval-formula
        "tE" 'org-table-export
        "th" 'org-table-previous-field
        "tH" 'org-table-move-column-left
        "tic" 'org-table-insert-column
        "tih" 'org-table-insert-hline
        "tiH" 'org-table-hline-and-move
        "tir" 'org-table-insert-row
        "tI" 'org-table-import
        "tj" 'org-table-next-row
        "tJ" 'org-table-move-row-down
        "tK" 'org-table-move-row-up
        "tl" 'org-table-next-field
        "tL" 'org-table-move-column-right
        "tn" 'org-table-create
        "tN" 'org-table-create-with-table.el
        "tr" 'org-table-recalculate
        "ts" 'org-table-sort-lines
        "ttf" 'org-table-toggle-formula-debugger
        "tto" 'org-table-toggle-coordinate-overlays
        "tw" 'org-table-wrap-region

        ;; Multi-purpose keys
        (or dotspacemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
        "*" 'org-ctrl-c-star
        "RET" 'org-ctrl-c-ret
        "-" 'org-ctrl-c-minus
        "^" 'org-sort
        "/" 'org-sparse-tree

        "I" 'org-clock-in
        "n" 'org-narrow-to-subtree
        "N" 'widen
        "O" 'org-clock-out
        "q" 'org-clock-cancel
        "R" 'org-refile
        "s" 'org-schedule

        ;; insertion of common elements
        "ia" 'org-attach
        "il" 'org-insert-link
        "if" 'org-footnote-new
        "ik" 'spacemacs/insert-keybinding-org

        ;; images and other link types have no commands in org mode-line
        ;; could be inserted using yasnippet?
        ;; region manipulation
        "xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
        "xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
        "xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
        "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
        "xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
        "xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
        "xv" (spacemacs|org-emphasize spacemacs/org-verbose ?=))

      ;; Add global evil-leader mappings. Used to access org-agenda
      ;; functionalities – and a few others commands – from any other mode.
      (spacemacs/declare-prefix "ao" "org")
      (spacemacs/set-leader-keys
        ;; org-agenda
        "ao#" 'org-agenda-list-stuck-projects
        "ao/" 'org-occur-in-agenda-files
        "aoa" 'org-agenda-list
        "aoe" 'org-store-agenda-views
        "aom" 'org-tags-view
        "aoo" 'org-agenda
        "aos" 'org-search-view
        "aot" 'org-todo-list
        ;; other
        "aoO" 'org-clock-out
        "aoc" 'org-capture
        "aol" 'org-store-link)

      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (define-key global-map "\C-cc" 'org-capture))

    :config
    (progn
      (setq org-default-notes-file (concat org-directory "/gtd/Note.org"))

      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (define-key org-src-mode-map
        (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '"))
        'org-edit-src-exit)

      (spacemacs/set-leader-keys "Cc" 'org-capture)

      ;; Evilify the calendar tool on C-c .
      (unless (eq 'emacs dotspacemacs-editing-style)
        (dpefine-key org-read-date-minibuffer-local-map (kbd "M-h")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-day 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-l")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-day 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-k")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-week 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-j")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-week 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-H")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-month 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-L")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-month 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-K")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-year 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-J")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-year 1)))))

      ;; load modules
      (setq org-modules
            '(;;;; org official lisps
              org-bbdb org-bibtex org-crypt org-docview
              org-habit org-id org-info org-man
              org-w3m org-protocol ;; org-gnus
              ;;;; org contribute lisps
              ;; NOTE: it is better to load it with `try-require' to avoid
              ;; problem when user don't have `org-plus-contrib' package
              ;; org-bookmark org-mew org-expiry org-git-link
              ))

      ;; todo items
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

      ;; todo hooks
      ;; todo entry automatically changes to DONE when all children are done
      ;; (defun org-summary-todo (n-done n-not-done)
      ;;   "Switch entry to DONE when all subentries are done, to TODO
      ;; otherwise."
      ;;   (let (org-log-done org-log-states)   ; turn off logging
      ;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
      ;; (add-hook ('org-after-todo-statistics-hook 'org-summary-todo)

      ;; - REF: http://thread.gmane.org/gmane.emacs.orgmode/21402/focus=21413
      ;; - FIXME: `state' variable is not recognised
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
                   ;; TODO: Add popup notification when done
                   ;; tips: use-package :if
                   ;; (when window-system
                   ;;   (when (try-require 'todochiku)
                   ;;     (if (string= org-state "DONE")
                   ;;         (todochiku-message "Emacs Org"
                   ;;                            "Task DONE, Great Work!"
                   ;;                            (todochiku-icon 'check)))))
                   ))

      ;; tags
      (setq org-stuck-projects '("+prj/-SOMEDAY-DONE" ("NEXT" "STARTED"))
            org-use-tag-inheritance t
            org-tags-exclude-from-inheritance '("prj" "crypt" "book"))
      ;; TODO: check if this works
      ;; (setq org-todo-state-tags-triggers
      ;;         '(("TODO"      ("new"))
      ;;           ("NEXT"      ("new"))
      ;;           ("STARTED"   ("new"))
      ;;           ("DONE"      ("new") ("old" . t))
      ;;           ("WAITING"   ("new"))
      ;;           ("SOMEDAY"   ("new"))
      ;;           ("CANCELLED" ("new") ("important") ("old" . t))))

      ;; global tags
      ;; - NOTE: it is better to define global tags in org files
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

      ;; properties
      ;; Don't inheritant property for sub-items, since it slows
      ;; down property searchings.
      (setq org-use-property-inheritance nil)

      ;; additional properties
      ;; - NOTE: a task should not takes more than 4 hours (a half day),
      ;; otherwise you MUST break it into smaller tasks.
      (setq org-global-properties
            '(("Effort_ALL" .
               "0:10 0:20 0:30 1:00 1:30 2:00 2:30 3:00 4:00")
              ("Importance_ALL" .
               "A B C")
              ("SCORE_ALL" .
               "0 1 2 3 4 5 6 7 8 9 10")))

      ;; priority
      (setq org-enable-priority-commands           t
            org-highest-priority                  ?A
            org-lowest-priority                   ?C
            org-default-priority                  ?B
            org-priority-start-cycle-with-default  t)

      ;; column view
      (setq org-columns-default-format ; Set default column view headings
        "%CATEGORY(Cat.) %PRIORITY(Pri.) %Importance(Imp.) \
%6TODO(State) %35ITEM(Details) %ALLTAGS(Tags) %5Effort(Plan){:} \
%6CLOCKSUM(Clock){Total} %SCORE(SCORE)")

      ;; clock
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

      ;; logging
      (setq org-log-done            'time
            org-log-done-with-time  t
            org-log-into-drawer     t
            org-log-redeadline      'note
            org-log-reschedule      'time
            org-log-refile          'time
            org-log-state-notes-insert-after-drawers t)

      ;; archieve
      ;; Infomation saved in archives
      (setq org-archive-save-context-info
            '(time file category todo priority itags olpath ltags))
      ;; Makes it possible to archive tasks that are not marked DONE
      (setq org-archive-mark-done nil)


      ;; TODO todochiku
      ;; - NOTE: already in `appt' setting
      ;; (when window-system
      ;;   (when (try-require 'todochiku)
      ;;     (setq org-show-notification-handler
      ;;           '(lambda (notification)
      ;;              (todochiku-message "org-mode notification" notification
      ;;                                 (todochiku-icon 'emacs))))))

      ;; capture
      ;; REF:
      ;; - https://github.com/sprig/org-capture-extension
      ;; - https://github.com/sprig/org-capture-extension/issues/37
      (defun transform-square-brackets-to-round-ones(string-to-transform)
        "Transforms [ into ( and ] into ), other chars left unchanged."
        (concat
         (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

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

;; ;; TODO: try with `org-contact'
;;               ("c" "Contacts"
;;                entry (file+headline "~/emacs/org/gtd/Contacts.org" "New Contacts")
;;                "** %(org-contacts-template-name)
;; :PROPERTIES:%(org-contacts-template-email)
;; :COMPANY:
;; :POSITION:
;; :MOBILE:
;; :PHONE:
;; :WECHAT/QQ:
;; :EMAIL:
;; :SKYPE/FACEBOOK:
;; :ADDRESS:
;; :NOTE:
;; :END:"
;;                :empty-lines 1 :prepend t :clock-keep t)

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

              ;; REF: https://github.com/sprig/org-capture-extension
              ;; chrome extension: org-capture-extension
              ("p" "Protocol"
               ;; entry (file+headline ,(concat org-directory "notes.org") "Inbox")
               ;; "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
               entry (file+headline "~/emacs/org/gtd/Note.org" "Note Inbox")
               "** NEW %^{Title} %^G\n- Source: %u, %c\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
               :empty-lines 1 :prepend t)

	            ("L" "Protocol Link"
               ;; entry (file+headline ,(concat org-directory "notes.org") "Inbox")
               ;; "* %? [[%:link][%:description]] \nCaptured On: %U")
               entry (file+headline "~/emacs/org/gtd/Note.org" "Note Inbox")
               "** NEW %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
               :empty-lines 1 :prepend t)
              ))

      ;; refile
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


      ;; src
      ;; (setq org-src-fontify-natively t) ;; already in :init
      (setq org-confirm-babel-evaluate nil
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

      ;; export
      (setq org-export-backends
            '(ascii beamer html latex md org odt freemind koma-letter))
      (setq org-file-apps ;; set default viewer for exported files
            '((auto-mode       . emacsclient)
              ("\\.x?html?\\'" . system)
              ;; use mupdf for normal viewing
              ;; ("\\.pdf\\'"     . "mupdf -b 8 -r 96 %s")
              ("\\.pdf\\'"     . "mupdf -r 96 %s")
              ;; use evice for viewing specific number of page
              ;; ("\\.pdf::\\(\\d+\\)\\'" . "evince -p %1 %s")
              ("\\.pdf::\\(\\d+\\)\\'" . "okular -p %1 %s")
              ("\\.pdf.xoj"    . "xournal %s")
              ("\\.png\\'"     . "display %s")
              ("\\.jpg\\'"     . "display %s")
              ("\\.bmp\\'"     . "display %s")
              ("\\.gif\\'"     . "display %s")
              ("\\.wav\\'"     . "play %s")
              ("\\.mp3\\'"     . "play %s")
              ("\\.aac\\'"     . "play %s")
              ("\\.flac\\'"    . "play %s")))
      ;; FIXME: there is a problem with history files, cannot start new emacs process for exporting
      ;; (setq org-export-in-background t) ;; use background export by default
      ;; Special symboles
      ;; check http://orgmode.org/manual/Special-symbols.html
      ;; Just use standard \nbsp{} instead of custom entities
      ;; (setq org-entities-user
      ;;       '(("sp" "~" nil "&nbsp;" " " " " " ") ;; non-breaking spaces
      ;;         ))

      )))

(defun org/init-ox-latex()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-latex))
  (use-package ox-latex
    :defer t
    :after ox
    :config
    (progn
      (setq org-latex-coding-system 'utf-8-unix
            org-latex-table-caption-above nil
            org-latex-tables-column-borders t
            ;; code listing settings, new `minted' is also supported
            org-latex-listings t
            ;; fix the bug of current version
            org-latex-preview-ltxpng-directory "./")

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
               ("\\subsection{%s}" . "\\subsection*{%s}")
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

              ;; NOTE: ctex documentclasses, no need to use ctex package
              ("ctexart" "\\documentclass[UTF8,winfonts,cs4size,a4paper,\
cap,punct,nospace,indent,fancyhdr,hypperref,fntef]{ctexart}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

              ("ctexrep" "\\documentclass[UTF8,winfonts,cs4size,a4paper,\
cap,punct,nospace,indent,fancyhdr,hypperref, fntef]{ctexrep}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

              ("ctexbook" "\\documentclass[UTF8,winfonts,cs4size,a4paper,\
cap,punct,nospace,indent,fancyhdr,hypperref,fntef]{ctexbook}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

              ;; nice layout, but not very useful
              ;; ("pracjourn" "\\documentclass{pracjourn}"
              ;;   ("\\section{%s}" . "\\section*{%s}")
              ;;   ("\\subsection{%s}" . "\\subsection*{%s}")
              ;;   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ;;   ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ;;   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
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
            '(:foreground default
                          :background default
                          :scale 1.0
                          :html-foreground "Black"
                          :html-background "Transparent"
                          :html-scale 1.0
                          :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

      (setq org-format-latex-signal-error t)
      (setq org-latex-create-formula-image-program 'imagemagick)

      ;; Use latexmk instead of xelatex
      (setq org-latex-pdf-process
            '("latexmk -pdf -bibtex -f -silent %b"
              "latexmk -c"))
      )))

(defun org/init-ox-beamer ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-beamer))
  (use-package ox-beamer
    :defer t
    :after (ox ox-latex)))

(defun org/init-ox-bibtex ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-bibtex))
  (use-package ox-bibtex
    :defer t
    :after (ox ox-latex)
    :ensure-system-package bibtex2html
    ))

(defun org/init-ox-html ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-html))
  (use-package ox-html
    :defer t
    :after ox
    :init
    (progn
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
        "Join consecutive Chinese lines into a single long line \
without unwanted space when exporting org-mode to html."
        (let ((fixed-contents)
              (orig-contents (ad-get-arg 1))
              (reg-han "[[:multibyte:]]"))
          (setq fixed-contents (replace-regexp-in-string
                                (concat "\\(" reg-han "\\) *\n *\\(" reg-han "\\)")
                                "\\1\\2" orig-contents))
          (ad-set-arg 1 fixed-contents)
          )))))

(defun org/init-ox-odt ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-odt))
  (use-package ox-odt
    :defer t
    :after ox
    :init
    (progn
      (setq org-odt-data-dir (concat org-directory "/addon/odt/styles")))))

(defun org/init-org-crypt ()
  (spacemacs|use-package-add-hook org :post-config (require 'org-crypt))
  (use-package org-crypt
    :defer t
    :after org
    :commands (org-crypt-use-before-save-magic)
    :config
    (progn
      ;; (org-crypt-use-before-save-magic)
      (setq org-use-tag-inheritance t   ;; Inherit tags in most of cases
            org-tags-exclude-from-inheritance (quote ("crypt" "prj" "book"))
            org-crypt-key nil)
      ;; (setq auto-save-default nil)
      ;; ;; Auto-saving does not cooperate with org-crypt.el: so you need
      ;; ;; to turn it off if you plan to use org-crypt.el quite often.
      ;; ;; Otherwise, you'll get an (annoying) message each time you
      ;; ;; start Org.
      ;; ;; To turn it off only locally, you can insert this:
      ;; ;;
      ;; ;; # -*- buffer-auto-save-file-name: nil; -*-
      )))

(defun org/init-org-attach ()
  (spacemacs|use-package-add-hook org :post-config (require 'org-attach))
  (use-package org-attach
    :defer t
    :init
    (progn
      (setq org-link-abbrev-alist
            '(("att" . org-attach-expand-link))))))

(defun org/init-org-agenda ()
  (use-package org-agenda
    :defer t
    :init
    (progn
      ;; Moved from org/init-org :config
      ;; FIXME: error when activate
      ;; ;;;; update appt each time agenda opened
      ;; (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
      ;; (defadvice  org-agenda-redo (after org-agenda-redo-add-appts)
      ;;   "Pressing `r' on the agenda will also add appointments."
      ;;   (progn
      ;;     (setq appt-time-msg-list nil)
      ;;     (org-agenda-to-appt)))
      ;; (ad-activate 'org-agenda-redo)
      (setq org-agenda-restore-windows-after-quit t
            org-agenda-log-mode-items '(closed state))
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        ":" 'org-agenda-set-tags
        "a" 'org-agenda
        "d" 'org-agenda-deadline
        "f" 'org-agenda-set-effort
        "I" 'org-agenda-clock-in
        "O" 'org-agenda-clock-out
        "P" 'org-agenda-set-property
        "q" 'org-agenda-refile
        "Q" 'org-agenda-clock-cancel
        "s" 'org-agenda-schedule)
      (spacemacs|define-transient-state org-agenda
      :title "Org-agenda transient state"
      :on-enter (setq which-key-inhibit t)
      :on-exit (setq which-key-inhibit nil)
      :foreign-keys run
      :doc
      "
Headline^^            Visit entry^^               Filter^^                    Date^^               Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------  -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule      [_tf_] follow        [_vd_] day         [_ci_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dd_] set deadline  [_tl_] log           [_vw_] week        [_co_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fc_] by category          [_dt_] timestamp     [_ta_] archive       [_vt_] fortnight   [_ck_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_+_]  do later      [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_hT_] set tags       ^^                          [_fx_] by regexp            [_-_]  do earlier    [_td_] diaries       [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   ^^                   ^^                   [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vr_] reset       ^^             ^^
[_q_] quit
"

      :bindings
      ;; Entry
      ("ht" org-agenda-todo)
      ("hk" org-agenda-kill)
      ("hr" org-agenda-refile)
      ("hA" org-agenda-archive-default)
      ("hT" org-agenda-set-tags)
      ("hp" org-agenda-priority)

      ;; Visit entry
      ("SPC" org-agenda-show-and-scroll-up)
      ("<tab>" org-agenda-goto :exit t)
      ("TAB" org-agenda-goto :exit t)
      ("RET" org-agenda-switch-to :exit t)
      ("o"   link-hint-open-link :exit t)

      ;; Date
      ("ds" org-agenda-schedule)
      ("dd" org-agenda-deadline)
      ("dt" org-agenda-date-prompt)
      ("+" org-agenda-do-date-later)
      ("-" org-agenda-do-date-earlier)

      ;; View
      ("vd" org-agenda-day-view)
      ("vw" org-agenda-week-view)
      ("vt" org-agenda-fortnight-view)
      ("vm" org-agenda-month-view)
      ("vy" org-agenda-year-view)
      ("vn" org-agenda-later)
      ("vp" org-agenda-earlier)
      ("vr" org-agenda-reset-view)

      ;; Toggle mode
      ("tf" org-agenda-follow-mode)
      ("tl" org-agenda-log-mode)
      ("ta" org-agenda-archives-mode)
      ("tr" org-agenda-clockreport-mode)
      ("td" org-agenda-toggle-diary)

      ;; Filter
      ("ft" org-agenda-filter-by-tag)
      ("fr" org-agenda-filter-by-tag-refine)
      ("fc" org-agenda-filter-by-category)
      ("fh" org-agenda-filter-by-top-headline)
      ("fx" org-agenda-filter-by-regexp)
      ("fd" org-agenda-filter-remove-all)

      ;; Clock
      ("ci" org-agenda-clock-in :exit t)
      ("co" org-agenda-clock-out)
      ("ck" org-agenda-clock-cancel)
      ("cj" org-agenda-clock-goto :exit t)

      ;; Other
      ("q" nil :exit t)
      ("gr" org-agenda-redo)
      ("." org-agenda-goto-today)
      ("gd" org-agenda-goto-date)))

    :config
    (progn
      (setq org-agenda-dim-blocked-tasks nil
            org-agenda-window-frame-fractions '(0.20 . 0.80)
            ;; org-agenda-restore-windows-after-quit t
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
            ;; FIXME: the custom time grid need to be fixed for this version of org
            ;; org-agenda-time-grid '((daily today require-timed) "----------------"
            ;;                        (800 1000 1200 1400 1600 1800 2000 2200))
            org-agenda-sorting-strategy
            '((agenda time-up category-keep priority-down todo-state-up)
              (todo time-up category-keep priority-down todo-state-up)
              (tags time-up category-keep priority-down todo-state-up)
              (search time-up category-keep priority-down todo-state-up)))

      ;; Custom agenda commands
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

                (tags-todo "TODO<>\"TODO\"+TODO<>\"SOMEDAY\"\
-SCHEDULED<=\"<+7d>\"-SCHEDULED>\"<+14d>\"-DEADLINE<=\"<+7d>\"-DEADLINE>\"<+14d>\"\
-repeat-bookmark-appt-note-en-prj"
                           ((org-agenda-overriding-header
                             "Pending Next Actions")
                            (org-tags-match-list-sublevels t)))

                (tags-todo "TODO=\"TODO\"-repeat"
                           ((org-agenda-overriding-header
                             "Task Inbox")
                            (org-tags-match-list-sublevels t)))

                (tags-todo "SCHEDULED>=\"<+1d>\"+SCHEDULED<=\"<+7d>\"\
-repeat-note-bookmark-en"
                           ((org-agenda-overriding-header
                             "Scheduled Tasks in 7 Days")
                            (org-tags-match-list-sublevels nil)))

                (tags-todo "TODO=\"SOMEDAY\"-sub"
                           ((org-agenda-overriding-header
                             "Future Work")
                            (org-tags-match-list-sublevels nil)))))

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

                (tags "prj/!-TODO-SOMEDAY-sub"
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
              ))

      ;; Agenda view export C-x C-w
      (setq org-agenda-exporter-settings
            '((ps-number-of-columns 2)
              (ps-landscape-mode t)
              ;; (org-agenda-prefix-format " [ ] ")
              ;; (org-agenda-with-colors nil)
              ;; (org-agenda-remove-tags t)
              (org-agenda-add-entry-text-maxlines 5)
              (htmlize-output-type 'css)))

      (evilified-state-evilify-map org-agenda-mode-map
        :mode org-agenda-mode
        :bindings
        "j" 'org-agenda-next-line
        "k" 'org-agenda-previous-line
        (kbd "M-j") 'org-agenda-next-item
        (kbd "M-k") 'org-agenda-previous-item
        (kbd "M-h") 'org-agenda-earlier
        (kbd "M-l") 'org-agenda-later
        (kbd "gd") 'org-agenda-toggle-time-grid
        (kbd "gr") 'org-agenda-redo
        (kbd "M-RET") 'org-agenda-show-and-scroll-up
        (kbd "M-SPC") 'spacemacs/org-agenda-transient-state/body
        (kbd "s-M-SPC") 'spacemacs/org-agenda-transient-state/body))))

(defun org/init-org-download ()
  (use-package org-download
    :commands (org-download-enable
               org-download-yank
               org-download-screenshot)
    :init
    (progn
      (add-hook 'org-mode-hook 'org-download-enable)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "iy" 'org-download-yank
        "is" 'org-download-screenshot))))

(defun org/init-org-mime ()
  (use-package org-mime
    :defer t
    :commands (org-mime-htmlize org-mime-org-buffer-htmlize)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'message-mode
        "M" 'org-mime-htmlize)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "m" 'org-mime-org-buffer-htmlize))))

(defun org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (spacemacs/system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "p" 'org-pomodoro)
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "p" 'org-pomodoro))))

(defun org/init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (evilified-state-evilify nil org-present-mode-keymap
        "h" 'org-present-prev
        "l" 'org-present-next
        "q" 'org-present-quit)
      (defun spacemacs//org-present-start ()
        "Initiate `org-present' mode"
        (org-present-big)
        (org-display-inline-images)
        (org-present-hide-cursor)
        (org-present-read-only)
        (evil-evilified-state))
      (defun spacemacs//org-present-end ()
        "Terminate `org-present' mode"
        (org-present-small)
        (org-remove-inline-images)
        (org-present-show-cursor)
        (org-present-read-write)
        (evil-normal-state))
      (add-hook 'org-present-mode-hook 'spacemacs//org-present-start)
      (add-hook 'org-present-mode-quit-hook 'spacemacs//org-present-end))))

(defun org/init-org-projectile ()
  (use-package org-projectile
    :commands (org-projectile-location-for-project)
    :init
    (progn
      (spacemacs/set-leader-keys
        "aop" 'org-projectile/capture
        "po" 'org-projectile/goto-todos)
      (require 'org-projectile))
    :config
    (if (file-name-absolute-p org-projectile-file)
        (progn
          (setq org-projectile-projects-file org-projectile-file)
          (push (org-projectile-project-todo-entry
                 nil nil nil :empty-lines 1)
                org-capture-templates))
      (org-projectile-per-project)
      (setq org-projectile-per-project-filepath org-projectile-file))))

(defun org/init-ox-twbs ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-twbs)))

(defun org/init-ox-gfm ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-gfm)))

(defun org/init-ox-reveal ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-reveal)))

(defun org/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@Org"
    :binding "o"
    :body
    (let ((agenda-files (org-agenda-files)))
      (if agenda-files
          (find-file (first agenda-files))
        (user-error "Error: No agenda files configured, nothing to display.")))))

;;; packages.el ends here
