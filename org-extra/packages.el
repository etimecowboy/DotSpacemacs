;;; packages.el --- org-extra layer packages file for Spacemacs.
;; Time-stamp: <2021-12-01 Wed 02:55 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq org-extra-packages
      '(
        (org :location built-in)
        (org-agenda :location built-in)
	      (org-crypt :location built-in)
	      (org-attach :location built-in)
        ob-async
        ;; ob-restclient ;; owned in restclient layer
        ob-ipython
        (ob-plantuml :location built-in)
        org-pdftools
        org-noter
        org-noter-pdftools
        ;; org-ref
        ;; org-brain
	      (ox-latex :location built-in)
	      (ox-beamer :location built-in)
	      (ox-bibtex :location built-in)
	      (ox-html :location built-in)
	      (ox-beamer :location built-in)
        ;; maxpix
        ;; equation to latex code generation, requires a payed appkey
        ;; - https://github.com/jethrokuan/mathpix.el
        ;; - https://accounts.mathpix.com/account
        ;; (maxthpix
        ;;  :location (recipe :fetcher github :repo "jethrokuan/mathpix.el"))
        ;; org-tanglesync ;; not very useful
        ;; polymode
        ;; (polybrain :location (recipe :fetcher github :repo "Kungsgeten/polybrain.el")) ;; not very useful
        ;; TODO: move to python(-extra) layer
        ;; conda ;; FIXME: should be in python layer too
        ;; anaconda-mode ;; FIXME: should be in python layer too
        ;; TODO: Add plantuml layer
        ;; plantuml-mode
        ;; flycheck-plantuml
        ;; TODO: Add graphviz layer
        ;; graphviz-dot-mode
        org-roam-ui
        ))

(defun org-extra/init-org ()
  (use-package org
    :defer (spacemacs/defer)
    :commands (orgtbl-mode org-clock-persistence-insinuate)
    :init
    (progn
      ;; copy from official spacemacs org-layer
      (spacemacs|require-when-dumping 'org)
      (setq org-log-done 'time
            org-startup-with-inline-images t
            org-latex-prefer-user-labels t
            ;; org-image-actual-width (/ (display-pixel-width) 4)
            ;; set to nil is good for customizing org mode image display
            ;; As per Jacobo's comment, add the following to your init.el file:
            ;; (setq org-image-actual-width nil)
            ;;
            ;; Then in org-mode, you can use this for inline previews of JPGs
            ;; and PNGs. Doesn't appear to work for SVGs (no idea why)
            ;;
            ;; #+ATTR_ORG: :width 100
            ;; [[~/images/example.jpg]]
            ;;
            ;; and if you want to size this for both inline previews and html output:
            ;;
            ;; #+ATTR_HTML: width="100px"
            ;; #+ATTR_ORG: :width 100
            ;; [[~/images/example.jpg]]
            org-image-actual-width nil
            org-src-fontify-natively t
            org-src-tab-acts-natively t
            ;; this is consistent with the value of
            ;; `helm-org-headings-max-depth'.
            org-imenu-depth 8)

      (with-eval-after-load 'org-agenda
        (add-to-list 'org-modules 'org-habit))

      (with-eval-after-load 'org-indent
        (spacemacs|hide-lighter org-indent-mode))

      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      ;; Follow the confirm and abort conventions
      (with-eval-after-load 'org-capture
        (defun spacemacs//org-capture-start ()
          "Make sure that the keybindings are available for org capture."
          (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
            dotspacemacs-major-mode-leader-key 'org-capture-finalize
            "a" 'org-capture-kill
            "c" 'org-capture-finalize
            "k" 'org-capture-kill
            "r" 'org-capture-refile)
          ;; Evil bindins seem not to be applied until at least one
          ;; Evil state is executed
          (evil-normal-state))
        ;; Must be done everytime we run org-capture otherwise it will
        ;; be ignored until insert mode is entered.
        (add-hook 'org-capture-mode-hook 'spacemacs//org-capture-start))

      (with-eval-after-load 'org-src
        (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
          dotspacemacs-major-mode-leader-key 'org-edit-src-exit
          "c" 'org-edit-src-exit
          "a" 'org-edit-src-abort
          "k" 'org-edit-src-abort))

      (autoload #'org-clock-jump-to-current-clock "org-clock")
      (add-hook 'org-mode-hook 'dotspacemacs//prettify-spacemacs-docs)

      (let ((dir (configuration-layer/get-layer-local-dir 'org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))

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

      (dolist (prefix '(
                        ("mb" . "babel")
                        ("mC" . "clocks")
                        ("md" . "dates")
                        ("me" . "export")
                        ("mf" . "feeds")
                        ("mi" . "insert")
                        ("miD" . "download")
                        ("mm" . "more")
                        ("ms" . "trees/subtrees")
                        ("mT" . "toggles")
                        ("mt" . "tables")
                        ("mtd" . "delete")
                        ("mti" . "insert")
                        ("mtt" . "toggle")
                        ("mx" . "text")
                        ))
        (spacemacs/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "'" 'org-edit-special
        "c" 'org-capture

        ;; Clock
        ;; These keybindings should match those under the "aoC" prefix (below)
        "Cc" 'org-clock-cancel
        "Cd" 'org-clock-display
        "Ce" 'org-evaluate-time-range
        "Cg" 'org-clock-goto
        "Ci" 'org-clock-in
        "CI" 'org-clock-in-last
        "Cj" 'spacemacs/org-clock-jump-to-current-clock
        "Co" 'org-clock-out
        "CR" 'org-clock-report
        "Cr" 'org-resolve-clocks

        "dd" 'org-deadline
        "ds" 'org-schedule
        "dt" 'org-time-stamp
        "dT" 'org-time-stamp-inactive
        "ee" 'org-export-dispatch
        "fi" 'org-feed-goto-inbox
        "fu" 'org-feed-update-all

        "a" 'org-agenda

        "p" 'org-priority

        "Tc" 'org-toggle-checkbox
        "Te" 'org-toggle-pretty-entities
        "Ti" 'org-toggle-inline-images
        "Tn" 'org-num-mode
        "Tl" 'org-toggle-link-display
        "Tt" 'org-show-todo-tree
        "TT" 'org-todo
        "TV" 'space-doc-mode
        "Tx" 'org-latex-preview

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
        "sa" 'org-toggle-archive-tag
        "sA" 'org-archive-subtree-default
        "sb" 'org-tree-to-indirect-buffer
        "sd" 'org-cut-subtree
        "sh" 'org-promote-subtree
        "sj" 'org-move-subtree-down
        "sk" 'org-move-subtree-up
        "sl" 'org-demote-subtree
        "sn" 'org-narrow-to-subtree
        "sw" 'widen
        "sr" 'org-refile
        "ss" 'org-sparse-tree
        "sS" 'org-sort

        ;; tables
        "ta" 'org-table-align
        "tb" 'org-table-blank-field
        "tc" 'org-table-convert
        "tdc" 'org-table-delete-column
        "tdr" 'org-table-kill-row
        "te" 'org-table-eval-formula
        "tE" 'org-table-export
        "tf" 'org-table-field-info
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
        "tR" 'org-table-recalculate-buffer-tables
        "ts" 'org-table-sort-lines
        "ttf" 'org-table-toggle-formula-debugger
        "tto" 'org-table-toggle-coordinate-overlays
        "tw" 'org-table-wrap-region

        ;; Source blocks / org-babel
        "bp"     'org-babel-previous-src-block
        "bn"     'org-babel-next-src-block
        "be"     'org-babel-execute-maybe
        "bo"     'org-babel-open-src-block-result
        "bv"     'org-babel-expand-src-block
        "bu"     'org-babel-goto-src-block-head
        "bg"     'org-babel-goto-named-src-block
        "br"     'org-babel-goto-named-result
        "bb"     'org-babel-execute-buffer
        "bs"     'org-babel-execute-subtree
        "bd"     'org-babel-demarcate-block
        "bt"     'org-babel-tangle
        "bf"     'org-babel-tangle-file
        "bc"     'org-babel-check-src-block
        "bj"     'org-babel-insert-header-arg
        "bl"     'org-babel-load-in-session
        "bi"     'org-babel-lob-ingest
        "bI"     'org-babel-view-src-block-info
        "bz"     'org-babel-switch-to-session
        "bZ"     'org-babel-switch-to-session-with-code
        "ba"     'org-babel-sha1-hash
        "bx"     'org-babel-do-key-sequence-in-edit-buffer
        "b."     'spacemacs/org-babel-transient-state/body
        ;; Multi-purpose keys
        (or dotspacemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
        "*" 'org-ctrl-c-star
        "-" 'org-ctrl-c-minus
        "#" 'org-update-statistics-cookies
        "RET"   'org-ctrl-c-ret
        "M-RET" 'org-meta-return
        ;; attachments
        "A" 'org-attach
        ;; insertion
        "ib" 'org-insert-structure-template
        "id" 'org-insert-drawer
        "ie" 'org-set-effort
        "if" 'org-footnote-new
        "ih" 'org-insert-heading
        "iH" 'org-insert-heading-after-current
        "ii" 'org-insert-item
        "iK" 'spacemacs/insert-keybinding-org
        "il" 'org-insert-link
        "in" 'org-add-note
        "ip" 'org-set-property
        "is" 'org-insert-subheading
        "it" 'org-set-tags-command
        ;; region manipulation
        "xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
        "xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
        "xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
        "xo" 'org-open-at-point
        "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
        "xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
        "xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
        "xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=))

      ;; Add global evil-leader mappings. Used to access org-agenda
      ;; functionalities – and a few others commands – from any other mode.
      (spacemacs/declare-prefix "ao" "org")
      (spacemacs/declare-prefix "aof" "feeds")
      (spacemacs/declare-prefix "aoC" "clock")
      ;; org-agenda
      (when (configuration-layer/layer-used-p 'ivy)
        (spacemacs/set-leader-keys "ao/" 'org-occur-in-agenda-files))
      (spacemacs/set-leader-keys
        "ao#" 'org-agenda-list-stuck-projects
        "aoa" 'org-agenda-list
        "aoo" 'org-agenda
        "aoc" 'org-capture
        "aoe" 'org-store-agenda-views
        "aofi" 'org-feed-goto-inbox
        "aofu" 'org-feed-update-all
        "ao]" 'org-remove-file
        "ao[" 'org-agenda-file-to-front

        ;; Clock
        ;; These keybindings should match those under the "mC" prefix (above)
        "aoCc" 'org-clock-cancel
        "aoCg" 'org-clock-goto
        "aoCi" 'org-clock-in
        "aoCI" 'org-clock-in-last
        "aoCj" 'spacemacs/org-clock-jump-to-current-clock
        "aoCo" 'org-clock-out
        "aoCr" 'org-resolve-clocks

        "aol" 'org-store-link
        "aom" 'org-tags-view
        "aos" 'org-search-view
        "aot" 'org-todo-list
        ;; SPC C- capture/colors
        "Cc" 'org-capture)

      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (define-key global-map "\C-cc" 'org-capture)

      ;; org files
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
                                                    "/org-timestamps/")))
    :config
    (progn
      ;; copy from offical spacemacs org-layer
      ;; Activate evil insert state after these commands.
      (dolist (fn '(org-insert-drawer
                    org-insert-heading
                    org-insert-item
                    org-insert-structure-template))
        (advice-add fn :after #'spacemacs//org-maybe-activate-evil-insert))

      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (define-key org-src-mode-map
        (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '"))
        'org-edit-src-exit)

      ;; Evilify the calendar tool on C-c .
      (unless (eq 'emacs dotspacemacs-editing-style)
        (define-key org-read-date-minibuffer-local-map (kbd "M-h")
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

      (spacemacs|define-transient-state org-babel
        :title "Org Babel Transient state"
        :doc "
[_j_/_k_] navigate src blocks         [_e_] execute src block
[_g_]^^   goto named block            [_'_] edit src block
[_z_]^^   recenter screen             [_q_] quit"
        :bindings
        ("q" nil :exit t)
        ("j" org-babel-next-src-block)
        ("k" org-babel-previous-src-block)
        ("g" org-babel-goto-named-src-block)
        ("z" recenter-top-bottom)
        ("e" org-babel-execute-maybe)
        ("'" org-edit-special :exit t))

      ;; added by myself
      (setq system-time-locale "C") ;; use standard timestamp
      ;; load modules
      (setq org-modules
            '(;;;; org official lisps
              ol-bbdb ol-bibtex ol-docview ol-info ol-man ol-w3m 
                      org-id org-crypt org-protocol org-habit
                      ;; ol-gnus org-bookmark org-mew org-expiry org-git-link
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
:END:
%i"
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
      (setq org-export-with-sub-superscripts '{})
      (setq org-file-apps ;; set default viewer for exported files
            '((auto-mode       . emacsclient)
              ("\\.x?html?\\'" . system)
              ;; use mupdf for normal viewing
              ;; ("\\.pdf\\'"     . "mupdf -b 8 -r 96 %s")
              ;; ("\\.pdf\\'"     . "mupdf -r 96 %s")
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

      ;; (defun shk-fix-inline-images ()
      ;;   (when org-inline-image-overlays
      ;;     (org-redisplay-inline-images)))
      ;; for newly-added images inline display
      ;; (add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)
      ;; (add-hook 'before-save-hook 'shk-fix-inline-images)
      ;; (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
      ;; (add-hook 'after-save-hook #'org-redisplay-inline-images)

      (require 'subr-x)
      (defun org+-babel-after-execute ()
        "Redisplay inline images after executing source blocks with graphics results."
        (when-let ((info (org-babel-get-src-block-info t))
                   (params (org-babel-process-params (nth 2 info)))
                   (result-params (cdr (assq :result-params params)))
                   ((member "graphics" result-params)))
          (org-display-inline-images)))

      (add-hook 'org-babel-after-execute-hook #'org+-babel-after-execute)
      (add-hook 'before-save-hook #'org-redisplay-inline-images)

      (defcustom org-inline-image-background nil
        "The color used as the default background for inline images.
When nil, use the default face background."
        :group 'org
        :type '(choice color (const nil)))

      (defun create-image-with-background-color (args)
        "Specify background color of Org-mode inline image through modify `ARGS'."
        (let* ((file (car args))
               (type (cadr args))
               (data-p (caddr args))
               (props (cdddr args)))
          ;; Get this return result style from `create-image'.
          (append (list file type data-p)
                  (list :background (or org-inline-image-background (face-background 'default)))
                  props)))

      (advice-add 'create-image :filter-args
                  #'create-image-with-background-color)
      )))


(defun org-extra/init-org-agenda ()
  (use-package org-agenda
    :defer t
    :init
    (progn
      (setq org-agenda-restore-windows-after-quit t)
      (dolist (prefix '(("mC" . "clocks")
                        ("md" . "dates")
                        ("mi" . "insert")
                        ("ms" . "trees/subtrees")))
        (spacemacs/declare-prefix-for-mode 'org-agenda-mode
          (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        (or dotspacemacs-major-mode-leader-key ",") 'org-agenda-ctrl-c-ctrl-c
        "a" 'org-agenda
        "c" 'org-agenda-capture
        "Cc" 'org-agenda-clock-cancel
        "Ci" 'org-agenda-clock-in
        "Co" 'org-agenda-clock-out
        "Cj" 'org-agenda-clock-goto
        "dd" 'org-agenda-deadline
        "ds" 'org-agenda-schedule
        "ie" 'org-agenda-set-effort
        "ip" 'org-agenda-set-property
        "iP" 'org-agenda-priority
        "it" 'org-agenda-set-tags
        "sr" 'org-agenda-refile)
      (spacemacs|define-transient-state org-agenda
        :title "Org-agenda transient state"
        :on-enter (setq which-key-inhibit t)
        :on-exit (setq which-key-inhibit nil)
        :evil-leader-for-mode (org-agenda-mode . ".")
        :foreign-keys run
        :doc
        "
Headline^^            Visit entry^^               Filter^^                    Date^^                  Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------     -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule         [_tf_] follow        [_vd_] day         [_cI_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dS_] un-schedule      [_tl_] log           [_vw_] week        [_cO_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fc_] by category          [_dd_] set deadline     [_ta_] archive       [_vt_] fortnight   [_cq_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_dD_] remove deadline  [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_h:_] set tags       ^^                          [_fx_] by regexp            [_dt_] timestamp        [_ti_] clock issues  [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   [_+_]  do later         [_td_] diaries       [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          [_-_]  do earlier       ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                      ^^                   [_vr_] reset       ^^             ^^
[_q_] quit
"
        :bindings
        ;; Entry
        ("h:" org-agenda-set-tags)
        ("hA" org-agenda-archive-default)
        ("hk" org-agenda-kill)
        ("hp" org-agenda-priority)
        ("hr" org-agenda-refile)
        ("ht" org-agenda-todo)

        ;; Visit entry
        ("SPC" org-agenda-show-and-scroll-up)
        ("<tab>" org-agenda-goto :exit t)
        ("TAB" org-agenda-goto :exit t)
        ("RET" org-agenda-switch-to :exit t)
        ("o"   link-hint-open-link :exit t)

        ;; Date
        ("+" org-agenda-do-date-later)
        ("-" org-agenda-do-date-earlier)
        ("dd" org-agenda-deadline)
        ("dD" (lambda () (interactive)
                (let ((current-prefix-arg '(4)))
                  (call-interactively 'org-agenda-deadline))))
        ("ds" org-agenda-schedule)
        ("dS" (lambda () (interactive)
                (let ((current-prefix-arg '(4)))
                  (call-interactively 'org-agenda-schedule))))
        ("dt" org-agenda-date-prompt)

        ;; View
        ("vd" org-agenda-day-view)
        ("vm" org-agenda-month-view)
        ("vn" org-agenda-later)
        ("vp" org-agenda-earlier)
        ("vr" org-agenda-reset-view)
        ("vt" org-agenda-fortnight-view)
        ("vw" org-agenda-week-view)
        ("vy" org-agenda-year-view)

        ;; Toggle mode
        ("ta" org-agenda-archives-mode)
        ("td" org-agenda-toggle-diary)
        ("tf" org-agenda-follow-mode)
        ("ti" org-agenda-show-clocking-issues)
        ("tl" org-agenda-log-mode)
        ("tr" org-agenda-clockreport-mode)

        ;; Filter
        ("fc" org-agenda-filter-by-category)
        ("fd" org-agenda-filter-remove-all)
        ("fh" org-agenda-filter-by-top-headline)
        ("fr" org-agenda-filter-by-tag-refine)
        ("ft" org-agenda-filter-by-tag)
        ("fx" org-agenda-filter-by-regexp)

        ;; Clock
        ("cI" org-agenda-clock-in :exit t)
        ("cj" org-agenda-clock-goto :exit t)
        ("cO" org-agenda-clock-out)
        ("cq" org-agenda-clock-cancel)

        ;; Other
        ("q" nil :exit t)
        ("gr" org-agenda-redo)
        ("." org-agenda-goto-today)
        ("gd" org-agenda-goto-date)))

    :config
    (progn
      ;; copy from offical spacemacs org-layer
      (evilified-state-evilify-map org-agenda-mode-map
        :mode org-agenda-mode
        :bindings
        "j" 'org-agenda-next-line
        "k" 'org-agenda-previous-line
        "K" nil
        ;; C-h should not be rebound by evilification so we unshadow it manually
        ;; TODO add the rule in auto-evilification to ignore C-h (like we do
        ;; with C-g)
        (kbd "C-h") nil
        (kbd "M-j") 'org-agenda-next-item
        (kbd "M-k") 'org-agenda-previous-item
        (kbd "M-h") 'org-agenda-earlier
        (kbd "M-l") 'org-agenda-later
        (kbd "gd") 'org-agenda-toggle-time-grid
        (kbd "gr") 'org-agenda-redo
        (kbd "M-RET") 'org-agenda-show-and-scroll-up
        (kbd "M-SPC") 'spacemacs/org-agenda-transient-state/body
        (kbd "s-M-SPC") 'spacemacs/org-agenda-transient-state/body)

      ;; added by myself
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
                        (;; (org-agenda-sorting-strategy '(priority-down))
                         (org-agenda-span 1)
                         (org-agenda-deadline-warning-days 14)
                         (org-agenda-use-time-grid t)
                         (org-agenda-skip-scheduled-if-done t)
                         (org-agenda-skip-deadline-if-done t)
                         (org-agenda-skip-timestamp-if-done t)
                         (org-agenda-skip-archived-trees t)
                         (org-agenda-skip-comment-trees t)
                         (org-agenda-todo-list-sublevel t)
                         (org-agenda-timeline-show-empty-dates t)))

;;                 (tags-todo "TODO<>\"TODO\"+TODO<>\"SOMEDAY\"\
;; -SCHEDULED<=\"<+7d>\"-SCHEDULED>\"<+14d>\"-DEADLINE<=\"<+7d>\"-DEADLINE>\"<+14d>\"\
;; -repeat-bookmark-appt-note-en-prj"
;;                            ((org-agenda-overriding-header
;;                              "Pending Next Actions")
;;                             (org-tags-match-list-sublevels t)))

                (tags-todo "TODO<>\"TODO\"+TODO<>\"SOMEDAY\"\
-repeat-bookmark-appt-note-en"
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
      )))


(defun org-extra/init-ox-beamer ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-beamer))
  (use-package ox-beamer
    :defer t
    :after (ox ox-latex)))


(defun org-extra/init-ox-bibtex ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-bibtex))
  (use-package ox-bibtex
    :defer t
    :after (ox ox-latex)
    ;;:ensure-system-package bibtex2html
    ))


(defun org-extra/init-ox-html ()
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

(defun org-extra/init-ox-odt ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-odt))
  (use-package ox-odt
    :defer t
    :after ox
    :init
    (progn
      (setq org-odt-data-dir (concat org-directory "/addon/odt/styles")))))


(defun org-extra/init-ox-latex()
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

(defun org-extra/init-org-crypt ()
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


(defun org-extra/init-org-attach ()
  (spacemacs|use-package-add-hook org :post-config (require 'org-attach))
  (use-package org-attach
    :defer t
    :init
    (progn
      (setq org-link-abbrev-alist
            '(("att" . org-attach-expand-link))))))


;; load ob-ipython
(defun org-extra/init-ob-ipython ()
  (use-package ob-ipython
    :defer t
    :after ob))


;; load ob-async
(defun org-extra/init-ob-async ()
  (use-package ob-async
    :defer t
    :after ob
    :config
    (progn
      (setq ob-async-no-async-languages-alist '("ipython")))))


;; load org-pdftools
(defun org-extra/init-org-pdftools ()
  (use-package org-pdftools
    :hook (org-load . org-pdftools-setup-link)))


;; load org-noter
(defun org-extra/init-org-noter ()
  (use-package org-noter
    :defer t
    :config
    (add-hook 'org-noter-insert-heading-hook #'org-id-get-create)
    (setq org-noter-auto-save-last-location nil
          org-noter-always-create-frame nil)
    (defun org-brain-open-org-noter (entry)
      "Open `org-noter' on the ENTRY.
If run interactively, get ENTRY from context."
      (interactive (list (org-brain-entry-at-pt)))
      (org-with-point-at (org-brain-entry-marker entry)
        (org-noter)))))


;; load org-noter-pdftools
(defun org-extra/init-org-noter-pdftools ()
  (use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions
              #'org-noter-pdftools-jump-to-note))))


;; load org-ref
;; (defun org-extra/init-org-ref ()
;;   (use-package org-ref
;;     ))

;; load org-brain ;; moved to org layer config
;; (defun org-extra/init-org-brain ()
;;   (spacemacs|use-package-add-hook org :post-config (require 'org-brain))
;;   (use-package org-brain
;;     :defer t
;;     :after org
;;     :init
;;     (progn
;;       ;; copy from offical spacemacs org-layer
;;       (spacemacs/declare-prefix "aoB" "org-brain")
;;       (spacemacs/set-leader-keys
;;         "aoBv" 'org-brain-visualize
;;         "aoBa" 'org-brain-agenda)
;;       (spacemacs/declare-prefix-for-mode 'org-mode "mB" "org-brain")
;;       (spacemacs/declare-prefix-for-mode 'org-mode "mBa" "add")
;;       (spacemacs/declare-prefix-for-mode 'org-mode "mBg" "goto")
;;       (spacemacs/set-leader-keys-for-major-mode 'org-mode
;;         "Bv" 'org-brain-visualize
;;         "Bac" 'org-brain-add-child
;;         "Bah" 'org-brain-add-child-headline
;;         "Bap" 'org-brain-add-parent
;;         "Bar" 'org-brain-add-resource
;;         "Baf" 'org-brain-add-friendship
;;         "Bgg" 'org-brain-goto
;;         "Bgc" 'org-brain-goto-child
;;         "Bgp" 'org-brain-goto-parent
;;         "Bgf" 'org-brain-goto-friend
;;         "BR"  'org-brain-refile
;;         "Bx"  'org-brain-delete-entry)
;;       (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
;;     :config
;;     (progn
;;       ;; added by myself
;;       ;; (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
;;       (setq org-brain-path (concat org-directory "/brain"))
;;       (setq org-id-track-globally t)
;;       ;; (setq org-id-locations-file (concat org-directory "/org-id-locations"))
;;       (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
;;       (push '("b" "Brain" plain (function org-brain-goto-end)
;;               "* %i%?" :empty-lines 1)
;;             org-capture-templates)
;;       (setq org-brain-visualize-default-choices 'root)
;;       (setq org-brain-title-max-length 30)
;;       (setq org-brain-include-file-entries t
;;             org-brain-file-entries-use-title t)
;;       (setq org-brain-scan-for-header-entries t)
;;       ;; (setq org-brain-default-file-parent "brain")
;;       (setq org-brain-scan-directories-recursively nil)
;;       (setq org-brain-backlink t)

;;       (defun org-brain-cliplink-resource ()
;;         "Add a URL from the clipboard as an org-brain resource . 
;; Suggest the URL title as a description for resource          . "
;;         (interactive)
;;         (let ((url (org-cliplink-clipboard-content)))
;;           (org-brain-add-resource
;;            url
;;            (org-cliplink-retrieve-title-synchronously url)
;;            t)))
;;       (define-key org-brain-visualize-mode-map (kbd "L") #'org-brain-cliplink-resource)

;;       ;; (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode)

;;       (defun helm-org-rifle-brain ()
;;         "Rifle files in `org-brain-path' . "
;;         (interactive)
;;         (let ((helm-org-rifle-close-unopened-file-buffers nil))
;;           (helm-org-rifle-directories (list org-brain-path))))
;;       ;; (add-to-list 'helm-org-rifle-actions
;;       ;;              (cons "Show entry in org-brain" 'helm-org-rifle-open-in-brain) t)
;;       )))

;; NOT good enough
;; ;; load org-tanglesync
;; (defun org/init-org-tanglesync ()
;;   (use-package org-tanglesync
;;     :hook ((org-mode . org-tanglesync-mode)
;;            ;; enable watch-mode globally:
;;            ((prog-mode text-mode) . org-tanglesync-watch-mode))
;;      :custom
;;      (org-tanglesync-watch-files '("conf.org" "myotherconf.org"))
;;      :config
;;      (spacemacs|diminish org-tanglesync-mode " Ȍ" " Ot")
;;      (spacemacs|diminish org-tanglesync-watch-mode " Ȏ" " Ow")
;;      ;; :bind
;;      ;; (("C-c M-i" . org-tanglesync-process-buffer-interactive)
;;      ;;  ("C-c M-a" . org-tanglesync-process-buffer-automatic))
;;      ))

;; (defun org/init-ploymode ()
;;   (use-package polymode))

;; TODO: move to graphviz layer
;; ;; load graphviz-dot-mode
;; (defun org/init-graphviz-dot-mode ()
;;   (use-package graphviz-dot-mode
;;     :defer t
;;     :after ob
;;     :config
;;     (progn
;;       (setq graphviz-dot-view-command "dotty %s"))))
;; TODO: move to plantuml layer
;; ;; load plantuml-mode
;; (defun org/init-plantuml-mode ()
;;   (use-package plantuml-mode
;;     :defer t
;;     :after ob
;;     :config
;;     (setq plantuml-jar-path (expand-file-name "~/opt/plantuml/plantuml.jar")
;;           plantuml-default-exec-mode 'jar)
;;     ))
;; load ob-plantuml
(defun org-extra/init-ob-plantuml ()
  (use-package ob-plantuml
    :defer t
    :after ob
    :config
    (setq org-plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\"")
          org-plantuml-jar-path (expand-file-name "~/opt/plantuml/plantuml.jar"))
    ))

;; TODO: move to python(-extra) layer
;; ;; load conda
;; (defun org/init-conda ()
;;   (use-package conda
;;     :defer t
;;     :after ob
;;     :config
;;     (progn
;;       (setq conda-anaconda-home "/opt/anaconda3/"
;;             conda-env-home-directory "~/.conda/"
;;             python-shell-virtualenv-root "~/.conda/envs")
;;       ;; (conda-env-initialize-interactive-shells)
;;       ;; (conda-env-autoactivate-mode t)
;;       ;; (conda-env-activate "py37_test") ;; not working
;;       )
;;     ;; :init
;;     ;; (progn
;;     ;;   ;; Fix org-capture json error
;;     ;; (conda-env-activate "py38_data")
;;     ;;   )
;;     ))

;; load org-roam-ui
(defun org-extra/init-org-roam-ui ()
  (use-package org-roam-ui))

;;; packages.el ends here
