;;; packages.el --- org layer packages file for Spacemacs.
;; Time-stamp: <2020-06-29 Mon 14:40 by xin on legion>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; My customized org layer that assist coding and documentation.
;; The shipped spacemacs org layer is shadowed by this one.

;;; Code:

(setq org-packages
      '(
        ;; My addon packges
        ; polymode
        ob-async
        ob-restclient
        ob-ipython
        conda ;; FIXME: should be in python layer too
        anaconda-mode ;; FIXME: should be in python layer too
        plantuml-mode
        flycheck-plantuml
        graphviz-dot-mode
        org-pdftools
        org-noter
        org-noter-pdftools
        org-tanglesync
        ;;------------------------------------------
        ;; Official org layer packages
        company
        company-emoji
        emoji-cheat-sheet-plus
        evil-org
        evil-surround
        gnuplot
        (helm-org :toggle (configuration-layer/layer-used-p 'helm))
        (helm-org-rifle :toggle (configuration-layer/layer-used-p 'helm))
        htmlize
        ;; ob, org and org-agenda are installed by `org-plus-contrib'
        (ob :location built-in)
        (org :location built-in)
        (org-agenda :location built-in)
        (org-brain :toggle (version<= "25" emacs-version))
        (org-expiry :location built-in)
        (org-journal :toggle org-enable-org-journal-support)
        org-download
        (org-jira :toggle org-enable-jira-support)
        org-mime
        org-pomodoro
        org-present
        org-cliplink
        (org-projectile :requires projectile)
        (ox-epub :toggle org-enable-epub-support)
        (ox-twbs :toggle org-enable-bootstrap-support)
        ;; use a for of ox-gfm to fix index generation
        (ox-gfm :location (recipe :fetcher github :repo "syl20bnr/ox-gfm")
                :toggle org-enable-github-support)
        (org-re-reveal :toggle org-enable-reveal-js-support)
        persp-mode
        (ox-hugo :toggle org-enable-hugo-support)
        (ox-jira :toggle org-enable-jira-support)
        (org-trello :toggle org-enable-trello-support)
        (org-sticky-header :toggle org-enable-sticky-header)
        (verb :toggle org-enable-verb-support)
        ))

(defun org/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf :modes org-mode))

(defun org/post-init-company-emoji ()
  (spacemacs|add-company-backends :backends company-emoji :modes org-mode))

(defun org/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'org-mode-hook 'spacemacs/delay-emoji-cheat-sheet-hook))

(defun org/init-evil-org ()
  (use-package evil-org
    :defer t
    :init
    (progn
      (add-hook 'org-mode-hook 'spacemacs//evil-org-mode)
      (setq evil-org-use-additional-insert t
            evil-org-key-theme `(textobjects
                                 navigation
                                 additional
                                 ,@(when org-want-todo-bindings '(todo)))))
    :config
    (spacemacs|hide-lighter evil-org-mode)))

(defun org/post-init-evil-surround ()
  (add-hook 'org-mode-hook 'spacemacs/org-setup-evil-surround))

(defun org/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'org-mode
            "tp" 'org-plot/gnuplot)))

(defun org/init-helm-org-rifle ()
  (use-package helm-org-rifle
    :after helm-org org-brain
    :defer t
    :init
    (spacemacs/set-leader-keys "aor" 'helm-org-rifle)
    (add-to-list 'helm-org-rifle-actions
                 (cons "Show entry in org-brain" 'helm-org-rifle-open-in-brain) t)
    ))

(defun org/init-helm-org ()
  (use-package helm-org
    :commands (helm-org-in-buffer-headings)
    :defer t))

(defun org/init-htmlize ()
  (use-package htmlize
    :defer t))

(defun org/init-ob ()
  (spacemacs|use-package-add-hook org :post-config (require 'ob))
  (use-package ob
    :defer t
    :init
    (progn
      (setq org-babel-load-languages
            '((emacs-lisp . t) (shell . t) (restclient . t)
              (ditaa . t) (dot . t) (plantuml . t) (gnuplot . t)
              (python . t) (ipython . t)
              ;; DONE <2019-08-22 17:43> gives an error when
              ;; ipython and jupyter are not installed
              ;; https://github.com/syl20bnr/spacemacs/issues/9941
              (perl . t) (ruby . t)
              (matlab . t) (octave . t)
              (C . t) (R . t)
              (latex . t) (org . t)
              ))

      (defun spacemacs//org-babel-do-load-languages ()
        "Load all the languages declared in `org-babel-load-languages'."
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages))
      (add-hook 'org-mode-hook 'spacemacs//org-babel-do-load-languages)
      ;; display/update images in the buffer after I evaluate
      (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
      ;; Fix redisplay of inline images after a code block evaluation.
      (add-hook 'org-babel-after-execute-hook 'spacemacs/ob-fix-inline-images)
      )
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
    :defer (spacemacs/defer)
    :commands (orgtbl-mode org-clock-persistence-insinuate)
    :init
    (progn
      (spacemacs|require 'org)
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
        "sA" 'org-archive-subtree
        "sb" 'org-tree-to-indirect-buffer
        "sd" 'org-cut-subtree
        "sh" 'org-promote-subtree
        "sj" 'org-move-subtree-down
        "sk" 'org-move-subtree-up
        "sl" 'org-demote-subtree
        "sn" 'org-narrow-to-subtree
        "sN" 'widen
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
      (spacemacs/set-leader-keys
        ;; org-agenda
        "ao#" 'org-agenda-list-stuck-projects
        "ao/" 'org-occur-in-agenda-files
        "aoa" 'org-agenda-list
        "aoc" 'org-capture
        "aoe" 'org-store-agenda-views
        "aofi" 'org-feed-goto-inbox
        "aofu" 'org-feed-update-all

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
        "aoo" 'org-agenda
        "aos" 'org-search-view
        "aot" 'org-todo-list
        ;; SPC C- capture/colors
        "Cc" 'org-capture)

      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (define-key global-map "\C-cc" 'org-capture))

    :config
    (progn
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
        ("e" org-babel-execute-maybe :exit t)
        ("'" org-edit-special :exit t))

      ;;--------------------------------------------
      ;; added by myself
      (setq system-time-locale "C") ;; use standard timestamp
      ;; load modules
      (setq org-modules
            '(;;;; org official lisps
              org-bbdb org-bibtex org-crypt org-docview
                       org-habit org-id org-info org-man
                       org-w3m org-protocol
                       ;; org-gnus org-bookmark org-mew org-expiry org-git-link
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

;;------------------------------------------------------------
;; added by myself
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
    ;;:ensure-system-package bibtex2html
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
  (spacemacs|use-package-add-hook org :post-config (require 'org-agenda))
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
      (dolist (prefix '(("mC" . "clocks")
                        ("md" . "dates")
                        ("mi" . "insert")
                        ("ms" . "trees/subtrees")))
        (spacemacs/declare-prefix-for-mode 'org-agenda-mode
          (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "a" 'org-agenda
        "Cc" 'org-agenda-clock-cancel
        "Ci" 'org-agenda-clock-in
        "Co" 'org-agenda-clock-out
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
        (kbd "s-M-SPC") 'spacemacs/org-agenda-transient-state/body)

      ;;------------------------------------------
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
      )))

(defun org/init-org-brain ()
  (spacemacs|use-package-add-hook org :post-config (require 'org-brain))
  (use-package org-brain
    :defer t
    :after org
    :init
    (progn
      (spacemacs/declare-prefix "aoB" "org-brain")
      (spacemacs/set-leader-keys
        "aoBv" 'org-brain-visualize
        "aoBa" 'org-brain-agenda)
      (spacemacs/declare-prefix-for-mode 'org-mode "mB" "org-brain")
      (spacemacs/declare-prefix-for-mode 'org-mode "mBa" "add")
      (spacemacs/declare-prefix-for-mode 'org-mode "mBg" "goto")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "Bv" 'org-brain-visualize
        "Bac" 'org-brain-add-child
        "Bah" 'org-brain-add-child-headline
        "Bap" 'org-brain-add-parent
        "Bar" 'org-brain-add-resource
        "Baf" 'org-brain-add-friendship
        "Bgg" 'org-brain-goto
        "Bgc" 'org-brain-goto-child
        "Bgp" 'org-brain-goto-parent
        "Bgf" 'org-brain-goto-friend
        "BR"  'org-brain-refile
        "Bx"  'org-brain-delete-entry)
      (evil-set-initial-state 'org-brain-visualize-mode 'emacs)
      (setq org-brain-path (concat org-directory "/brain"))
      (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
      )
    :config
    (progn
      (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
      (setq org-id-track-globally t)
      ;; (setq org-id-locations-file (concat org-directory "/org-id-locations"))
      (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
      (push '("b" "Brain" plain (function org-brain-goto-end)
              "* %i%?" :empty-lines 1)
            org-capture-templates)
      (setq org-brain-visualize-default-choices 'root)
      (setq org-brain-title-max-length 30)
      (setq org-brain-include-file-entries t
            org-brain-file-entries-use-title t)
      (setq org-brain-scan-for-header-entries t)
      ;; (setq org-brain-default-file-parent "brain")
      (setq org-brain-scan-directories-recursively nil)
      (setq org-brain-backlink t)


      (defun org-brain-cliplink-resource ()
        "Add a URL from the clipboard as an org-brain resource . 
Suggest the URL title as a description for resource          . "
        (interactive)
        (let ((url (org-cliplink-clipboard-content)))
          (org-brain-add-resource
           url
           (org-cliplink-retrieve-title-synchronously url)
           t)))
      (define-key org-brain-visualize-mode-map (kbd "L") #'org-brain-cliplink-resource)

      ;; (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode)

      (defun helm-org-rifle-brain ()
        "Rifle files in `org-brain-path' . "
        (interactive)
        (let ((helm-org-rifle-close-unopened-file-buffers nil))
          (helm-org-rifle-directories (list org-brain-path))))
      ;; (add-to-list 'helm-org-rifle-actions
      ;;              (cons "Show entry in org-brain" 'helm-org-rifle-open-in-brain) t)
      )))

(defun org/init-org-expiry ()
  (use-package org-expiry
    :commands (org-expiry-insinuate
               org-expiry-deinsinuate
               org-expiry-insert-created
               org-expiry-insert-expiry
               org-expiry-add-keyword
               org-expiry-archive-subtree
               org-expiry-process-entry
               org-expiry-process-entries)))

(defun org/init-org-download ()
  (use-package org-download
    :commands (org-download-enable
               org-download-yank
               org-download-screenshot)
    :init
    (progn
      (add-hook 'org-mode-hook 'org-download-enable)
      (spacemacs/declare-prefix-for-mode 'org-mode "miD" "download")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "iDy" 'org-download-yank
        "iDs" 'org-download-screenshot))))

(defun org/init-org-jira ()
  (use-package org-jira
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "aoJ" "jira")
      (spacemacs/declare-prefix "aoJp" "projects")
      (spacemacs/declare-prefix "aoJi" "issues")
      (spacemacs/declare-prefix "aoJs" "subtasks")
      (spacemacs/declare-prefix "aoJc" "comments")
      (spacemacs/declare-prefix "aoJt" "todos")
      (spacemacs/set-leader-keys
        "aoJpg" 'org-jira-get-projects
        "aoJib" 'org-jira-browse-issue
        "aoJig" 'org-jira-get-issues
        "aoJih" 'org-jira-get-issues-headonly
        "aoJif" 'org-jira-get-issues-from-filter-headonly
        "aoJiu" 'org-jira-update-issue
        "aoJiw" 'org-jira-progress-issue
        "aoJir" 'org-jira-refresh-issue
        "aoJic" 'org-jira-create-issue
        "aoJiy" 'org-jira-copy-current-issue-key
        "aoJsc" 'org-jira-create-subtask
        "aoJsg" 'org-jira-get-subtasks
        "aoJcu" 'org-jira-update-comment
        "aoJtj" 'org-jira-todo-to-jira))))

(defun org/init-org-mime ()
  (use-package org-mime
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'message-mode
        "em" 'org-mime-htmlize)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "em" 'org-mime-org-buffer-htmlize
        "es" 'org-mime-org-subtree-htmlize))))

(defun org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (spacemacs/system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "Cp" 'org-pomodoro)
      (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
        "Cp" 'org-pomodoro)
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "Cp" 'org-pomodoro))))

(defun org/init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (defun spacemacs//org-present-start ()
        "Initiate `org-present' mode"
        (org-present-big)
        (org-display-inline-images)
        (org-present-hide-cursor)
        (org-present-read-only)
        (evil-define-key 'normal org-present-mode-keymap
          "h"             'org-present-prev
          (kbd "<left>")  'org-present-prev
          "l"             'org-present-next
          (kbd "<right>") 'org-present-next
          "q"             'org-present-quit)
        ;; evil-normal-state seems to be required to load the above key bindings
        (evil-normal-state))
      (defun spacemacs//org-present-end ()
        "Terminate `org-present' mode"
        (org-present-small)
        (if (not org-startup-with-inline-images)
            (org-remove-inline-images))
        (org-present-show-cursor)
        (org-present-read-write))
      (add-hook 'org-present-mode-hook 'spacemacs//org-present-start)
      (add-hook 'org-present-mode-quit-hook 'spacemacs//org-present-end))))

(defun org/init-org-cliplink ()
  (use-package org-cliplink
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "iL" 'org-cliplink)))

(defun org/init-org-projectile ()
  (use-package org-projectile
    :commands (org-projectile-location-for-project)
    :init
    (progn
      (spacemacs/set-leader-keys
        "aop" 'org-projectile/capture
        "po" 'org-projectile/goto-todos)
      (with-eval-after-load 'org-capture
        (require 'org-projectile)))
    :config
    (if (file-name-absolute-p org-projectile-file)
        (progn
          (setq org-projectile-projects-file org-projectile-file)
          (push (org-projectile-project-todo-entry :empty-lines 1)
                org-capture-templates))
      (org-projectile-per-project)
      (setq org-projectile-per-project-filepath org-projectile-file))))

(defun org/pre-init-ox-epub ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-epub)))
(defun org/init-ox-epub ())

(defun org/pre-init-ox-twbs ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-twbs)))
(defun org/init-ox-twbs ())

(defun org/pre-init-ox-gfm ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-gfm)))
(defun org/init-ox-gfm ())

(defun org/pre-init-org-re-reveal ()
  (spacemacs|use-package-add-hook org :post-config (require 'org-re-reveal)))
(defun org/init-org-re-reveal ())

(defun org/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@Org"
    :binding "o"
    :body
    (let ((agenda-files (org-agenda-files)))
      (if agenda-files
          (find-file (first agenda-files))
        (user-error "Error: No agenda files configured, nothing to display.")))))

(defun org/init-org-journal ()
  (use-package org-journal
    :defer t
    :commands (org-journal-new-entry org-journal-search-forever)
    :init
    (progn
      (spacemacs/declare-prefix "aoj" "org-journal")
      (spacemacs/set-leader-keys
        "aojj" 'org-journal-new-entry
        "aojs" 'org-journal-search-forever
        "aojt" 'org-journal-new-scheduled-entry
        "aojv" 'org-journal-schedule-view)

      (setq spacemacs-org-journal-mode-map (copy-keymap spacemacs-org-mode-map))

      (spacemacs/set-leader-keys-for-major-mode 'calendar-mode
        "r" 'org-journal-read-entry
        "i" 'org-journal-new-date-entry
        "n" 'org-journal-next-entry
        "p" 'org-journal-previous-entry
        "s" 'org-journal-search-forever
        "w" 'org-journal-search-calendar-week
        "m" 'org-journal-search-calendar-month
        "y" 'org-journal-search-calendar-year)

      (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
        "j" 'org-journal-new-entry
        "n" 'org-journal-open-next-entry
        "p" 'org-journal-open-previous-entry)

      (spacemacs//init-leader-mode-map 'org-journal-mode 'spacemacs-org-journal-mode-map))))

(defun org/init-ox-hugo ()
  (use-package ox-hugo :after ox))

(defun org/init-ox-jira ()
  (use-package ox-jira :after ox))

(defun org/init-org-trello ()
  (use-package org-trello
    :after org
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'org-mode "mmt" "trello")
      (spacemacs/declare-prefix-for-mode 'org-mode "mmtd" "sync down")
      (spacemacs/declare-prefix-for-mode 'org-mode "mmtu" "sync up")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "mtI" 'org-trello-install-key-and-token
        "mta" 'org-trello-archive-card
        "mtc" 'org-trello-create-board-and-install-metadata
        "mti" 'org-trello-install-board-metadata
        "mtm" 'org-trello-update-board-metadata
        "mtdb" 'spacemacs/org-trello-pull-buffer
        "mtdc" 'spacemacs/org-trello-pull-card
        "mtub" 'spacemacs/org-trello-push-buffer
        "mtuc" 'spacemacs/org-trello-push-card))))

(defun org/init-org-sticky-header ()
  (use-package org-sticky-header
    :defer t
    :init
    (add-hook 'org-mode-hook 'org-sticky-header-mode)))

(defun org/init-verb ()
  (use-package verb
    :defer t
    :init
      (spacemacs/set-leader-keys-for-major-mode
        'org-mode
        "rf" #'verb-send-request-on-point
        "rs" #'verb-send-request-on-point-other-window
        "rr" #'verb-send-request-on-point-other-window-stay
        "rm" #'verb-send-request-on-point-no-window
        "rk" #'verb-kill-all-response-buffers
        "re" #'verb-export-request-on-point
        "ru" #'verb-export-request-on-point-curl
        "rb" #'verb-export-request-on-point-verb
        "rv" #'verb-set-var)
    :config
    (progn
      (spacemacs/set-leader-keys-for-minor-mode
        'verb-response-body-mode
        "rr" #'verb-toggle-show-headers
        "rk" #'verb-kill-response-buffer-and-window
        "rf" #'verb-re-send-request)
      (spacemacs/set-leader-keys-for-major-mode
        'verb-response-headers-mode
        "rq" #'verb-kill-buffer-and-window))))

(defun org/pre-init-verb ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(verb . t))))

;;-------------------------------------------------------------
;; added by myself
;; load graphviz-dot-mode
(defun org/init-graphviz-dot-mode ()
  (use-package graphviz-dot-mode
    :defer t
    :after ob
    :config
    (progn
      (setq graphviz-dot-view-command "dotty %s"))))

;; load ob-ipython
(defun org/init-ob-ipython ()
  (use-package ob-ipython
    :defer t
    :after ob))

;; load plantuml-mode
(defun org/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    :after ob
    :config
    (setq plantuml-jar-path (expand-file-name "~/opt/plantuml/plantuml.jar")
          plantuml-default-exec-mode 'jar)
    ))

;; load conda
(defun org/init-conda ()
  (use-package conda
    :defer t
    :after ob
    :config
    (progn
      (setq conda-anaconda-home "/opt/anaconda3/"
            conda-env-home-directory "~/.conda/"
            python-shell-virtualenv-root "~/.conda/envs")
      ;; (conda-env-initialize-interactive-shells)
      ;; (conda-env-autoactivate-mode t)
      ;; (conda-env-activate "py37_test") ;; not working
      )
    ;; :init
    ;; (progn
    ;;   ;; Fix org-capture json error
    ;; (conda-env-activate "py37_test")
    ;;   )
    ))

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

;; load org-pdftools
(defun org/init-org-pdftools ()
  (use-package org-pdftools
    :hook (org-load . org-pdftools-setup-link)))

;; load org-noter
(defun org/init-org-noter ()
  (use-package org-noter
    :defer t
    :config
    (add-hook 'org-noter-insert-heading-hook #'org-id-get-create)
    (defun org-brain-open-org-noter (entry)
      "Open `org-noter' on the ENTRY.
If run interactively, get ENTRY from context."
      (interactive (list (org-brain-entry-at-pt)))
      (org-with-point-at (org-brain-entry-marker entry)
        (org-noter)))))

;; load org-noter-pdftools
(defun org/init-org-noter-pdftools ()
  (use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions
              #'org-noter-pdftools-jump-to-note))))

;; load org-tanglesync
(defun org/init-org-tanglesync ()
  (use-package org-tanglesync
    :hook ((org-mode . org-tanglesync-mode)
           ;; enable watch-mode globally:
           ((prog-mode text-mode) . org-tanglesync-watch-mode))
     :custom
     (org-tanglesync-watch-files '("conf.org" "myotherconf.org"))
     :config
     (spacemacs|diminish org-tanglesync-mode " Ȍ" " Ot")
     (spacemacs|diminish org-tanglesync-watch-mode " Ȏ" " Ow")
     :bind
     (("C-c M-i" . org-tanglesync-process-buffer-interactive)
      ("C-c M-a" . org-tanglesync-process-buffer-automatic))))

;; (defun org/init-ploymode ()
;;   (use-package polymode))

;;; packages.el ends here
