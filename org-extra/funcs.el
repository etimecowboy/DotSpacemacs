;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- Org-extra Layer functions File for Spacemacs
;; Time-stamp: <2024-04-21 Sun 07:16 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; -- Fix table.el table alignment ---------------------------------------------

;; 如果你在 orgmode 中使用 table 表格，然后设置了固定宽度 width，然后发现长段落
;; 中文隐藏后无法对齐了，那么你可以重定义 org-table-align 函数，以实现中文对齐。
;; - https://gist.github.com/VitoVan/9dc2020cfb1e02f9a5a68031cb9979b5
(defun xy/org-table-align ()
  "Align the table at point by aligning all vertical bars."
  (interactive)
  (let* (
   ;; Limits of table
   (beg (org-table-begin))
   (end (org-table-end))
   ;; Current cursor position
   (linepos (org-current-line))
   (colpos (org-table-current-column))
   (winstart (window-start))
   (winstartline (org-current-line (min winstart (1- (point-max)))))
   lines (new "") lengths l typenums ty fields maxfields i
   column
   (indent "") cnt frac
   rfmt hfmt
   (spaces '(1 . 1))
   (sp1 (car spaces))
   (sp2 (cdr spaces))
   (rfmt1 (concat
     (make-string sp2 ?\ ) "%%%s%ds" (make-string sp1 ?\ ) "|"))
   (hfmt1 (concat
     (make-string sp2 ?-) "%s" (make-string sp1 ?-) "+"))
   emptystrings links dates emph raise narrow
   falign falign1 fmax f1 len c e space)
    (untabify beg end)
    (remove-text-properties beg end '(org-cwidth t org-dwidth t display t))
    ;; Check if we have links or dates
    (goto-char beg)
    (setq links (re-search-forward org-bracket-link-regexp end t))
    (goto-char beg)
    (setq emph (and org-hide-emphasis-markers
        (re-search-forward org-emph-re end t)))
    (goto-char beg)
    (setq raise (and org-use-sub-superscripts
         (re-search-forward org-match-substring-regexp end t)))
    (goto-char beg)
    (setq dates (and org-display-custom-times
         (re-search-forward org-ts-regexp-both end t)))
    ;; Make sure the link properties are right
    (when links (goto-char beg) (while (org-activate-bracket-links end)))
    ;; Make sure the date properties are right
    (when dates (goto-char beg) (while (org-activate-dates end)))
    (when emph (goto-char beg) (while (org-do-emphasis-faces end)))
    (when raise (goto-char beg) (while (org-raise-scripts end)))

    ;; Check if we are narrowing any columns
    (goto-char beg)
    (setq narrow (and org-table-do-narrow
          org-format-transports-properties-p
          (re-search-forward "<[lrc]?[0-9]+>" end t)))
    (goto-char beg)
    (setq falign (re-search-forward "<[lrc][0-9]*>" end t))
    (goto-char beg)
    ;; Get the rows
    (setq lines (org-split-string
     (buffer-substring beg end) "\n"))
    ;; Store the indentation of the first line
    (if (string-match "^ *" (car lines))
  (setq indent (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
    ;; Mark the hlines by setting the corresponding element to nil
    ;; At the same time, we remove trailing space.
    (setq lines (mapcar (lambda (l)
        (if (string-match "^ *|-" l)
            nil
          (if (string-match "[ \t]+$" l)
        (substring l 0 (match-beginning 0))
            l)))
      lines))
    ;; Get the data fields by splitting the lines.
    (setq fields (mapcar
      (lambda (l)
        (org-split-string l " *| *"))
      (delq nil (copy-sequence lines))))
    ;; How many fields in the longest line?
    (condition-case nil
  (setq maxfields (apply 'max (mapcar 'length fields)))
      (error
       (kill-region beg end)
       (org-table-create org-table-default-size)
       (user-error "Empty table - created default table")))
    ;; A list of empty strings to fill any short rows on output
    (setq emptystrings (make-list maxfields ""))
    ;; Check for special formatting.
    (setq i -1)
    (while (< (setq i (1+ i)) maxfields)   ;; Loop over all columns
      (setq column (mapcar (lambda (x) (or (nth i x) "")) fields))
      ;; Check if there is an explicit width specified
      (setq fmax nil)
      (when (or narrow falign)
  (setq c column fmax nil falign1 nil)
  (while c
    (setq e (pop c))
    (when (and (stringp e) (string-match "^<\\([lrc]\\)?\\([0-9]+\\)?>$" e))
      (if (match-end 1) (setq falign1 (match-string 1 e)))
      (if (and org-table-do-narrow (match-end 2))
    (setq fmax (string-to-number (match-string 2 e)) c nil))))
  ;; Find fields that are wider than fmax, and shorten them
  (when fmax
    (loop for xx in column do
    (when (and (stringp xx)
         (> (org-string-width xx) fmax))
      (org-add-props xx nil
        'help-echo
        (concat "Clipped table field, use C-c ` to edit.  Full value is:\n" (org-no-properties (copy-sequence xx))))
      (setq f1 (min fmax (or (string-match org-bracket-link-regexp xx) fmax)))
      (unless (> f1 1)
        (user-error "Cannot narrow field starting with wide link \"%s\""
                                (match-string 0 xx)))
                  (setq org-narrow-column-content "")
                  (dotimes (i (length xx))
                    (let* ((curr-string (char-to-string (aref xx i)))
                           (curr-width (string-width curr-string)))
                      (if (< (+ (string-width org-narrow-column-content) curr-width) fmax)
                          (setq org-narrow-column-content (concat org-narrow-column-content curr-string)))))
                  (setq org-narrow-column-content (concat org-narrow-column-content
                                                          (make-string (- fmax (string-width org-narrow-column-content)) ?.)))
      (add-text-properties 0 (length xx) (list 'org-cwidth t) xx)
      (add-text-properties 0 (min f1 (length xx))
                                       (list 'display org-narrow-column-content)
               xx)))))
      ;; Get the maximum width for each column
      (push (apply 'max (or fmax 1) 1 (mapcar 'org-string-width column))
      lengths)
      ;; Get the fraction of numbers, to decide about alignment of the column
      (if falign1
    (push (equal (downcase falign1) "r") typenums)
  (setq cnt 0 frac 0.0)
  (loop for x in column do
        (if (equal x "")
      nil
    (setq frac ( / (+ (* frac cnt)
          (if (string-match org-table-number-regexp x) 1 0))
             (setq cnt (1+ cnt))))))
  (push (>= frac org-table-number-fraction) typenums)))
    (setq lengths (nreverse lengths) typenums (nreverse typenums))

    ;; Store the alignment of this table, for later editing of single fields
    (setq org-table-last-alignment typenums
    org-table-last-column-widths lengths)

    ;; With invisible characters, `format' does not get the field width right
    ;; So we need to make these fields wide by hand.
    (when (or links emph raise)
      (loop for i from 0 upto (1- maxfields) do
      (setq len (nth i lengths))
      (loop for j from 0 upto (1- (length fields)) do
      (setq c (nthcdr i (car (nthcdr j fields))))
      (if (and (stringp (car c))
         (or (text-property-any 0 (length (car c))
              'invisible 'org-link (car c))
             (text-property-any 0 (length (car c))
              'org-dwidth t (car c)))
         (< (org-string-width (car c)) len))
          (progn
      (setq space (make-string (- len (org-string-width (car c))) ?\ ))
      (setcar c (if (nth i typenums)
              (concat space (car c))
            (concat (car c) space))))))))

    ;; Compute the formats needed for output of the table
    (setq rfmt (concat indent "|") hfmt (concat indent "|"))
    (while (setq l (pop lengths))
      (setq ty (if (pop typenums) "" "-")) ; number types flushright
      (setq rfmt (concat rfmt (format rfmt1 ty l))
      hfmt (concat hfmt (format hfmt1 (make-string l ?-)))))
    (setq rfmt (concat rfmt "\n")
    hfmt (concat (substring hfmt 0 -1) "|\n"))

    (setq new (mapconcat
         (lambda (l)
     (if l (apply 'format rfmt
            (append (pop fields) emptystrings))
       hfmt))
         lines ""))
    (move-marker org-table-aligned-begin-marker (point))
    (insert new)
    ;; Replace the old one
    (delete-region (point) end)
    (move-marker end nil)
    (move-marker org-table-aligned-end-marker (point))
    (when (and orgtbl-mode (not (derived-mode-p 'org-mode)))
      (goto-char org-table-aligned-begin-marker)
      (while (org-hide-wide-columns org-table-aligned-end-marker)))
    ;; Try to move to the old location
    (org-goto-line winstartline)
    (setq winstart (point-at-bol))
    (org-goto-line linepos)
    (when (eq (window-buffer (selected-window)) (current-buffer))
      (set-window-start (selected-window) winstart 'noforce))
    (org-table-goto-column colpos)
    (and org-table-overlay-coordinates (org-table-overlay-coordinates))
    (setq org-table-may-need-update nil)
    ))

;; -- Fix Org mode + table.el: Use Org formatting inside cells --------------------------
;; REF: https://emacs.stackexchange.com/questions/53195/org-mode-table-el-use-org-formatting-inside-cells
(defcustom org-html-tableel-org "no"
  "Export table.el cells as org code if set to \"t\" or \"yes\".
This is the default and can be changed per section with export option:
#+OPTIONS: HTML_TABLEEL_ORG: t"
  :type '(choice (const "no") (const "yes"))
  :group 'org-html)

(eval-after-load 'ox-html
  '(eval ;;< Avoid eager macro expansion before ox-html is loaded.
    '(cl-pushnew
      (list
       :html-tableel-org
       "HTML_TABLEEL_ORG" ;; keyword
       "html-tableel-org";; option for #+OPTIONS: line
       org-html-tableel-org ;; default value for the property
       t ;; handling of multiple keywords for the same property. (Replace old value with new one.)
       )
      (org-export-backend-options (org-export-get-backend 'html)))))

(defvar org-element-all-elements) ;; defined in "org-element"

(defun table-generate-orghtml-cell-contents (dest-buffer language cell info)
  "Generate and insert source cell contents of a CELL into DEST-BUFFER.
LANGUAGE must be 'orghtml."
  (cl-assert (eq language 'html) nil
             "Table cells with org content only working with html export")
  (let* ((cell-contents (extract-rectangle (car cell) (cdr cell)))
         (string (with-temp-buffer
                   (table--insert-rectangle cell-contents)
                   (table--remove-cell-properties (point-min) (point-max))
                   (goto-char (point-min))
                   (buffer-substring (point-min) (point-max)))))
    (with-current-buffer dest-buffer
      (let ((beg (point)))
        (insert (org-export-string-as string 'html t info))
        (indent-rigidly beg (point) 6)))))


(defun org-orghtml-table--table.el-table (fun table info)
  "Format table.el TABLE into HTML.
This is an advice for `org-html-table--table.el-table' as FUN.
INFO is a plist used as a communication channel."
  (if (assoc-string (plist-get info :html-tableel-org) '("t" "yes"))
      (cl-letf (((symbol-function 'table--generate-source-cell-contents)
                 (lambda (dest-buffer language cell)
                   (table-generate-orghtml-cell-contents dest-buffer language cell info))))
        (funcall fun table info))
    (funcall fun table info)))

(advice-add #'org-html-table--table.el-table :around #'org-orghtml-table--table.el-table)

;; -- create org-roam node silently ------------------------------------------------------

;; REF: https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/

(defun xy/org-roam-node-insert-immediate (arg &rest args)
  "Create a new node and insert a link in the current document without opening the new node's buffer."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; --------------------------------------------------------------------------------------

(defun xy/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))


(defun xy/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (xy/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))


(defun xy/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files
        (delete-dups (xy/org-roam-list-notes-by-tag "PROJECT"))))


(defun xy/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'xy/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))


(defun xy/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'xy/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (xy/org-roam-filter-by-tag "PROJECT")
   :templates
   '(("p" "project" plain "** TODO %?"
      :target
      (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                     "#+title: ${title}
#+category: ${title}
#+filetags: PROJECT
* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:
" ("Tasks"))))))


(defun xy/org-roam-find-hub ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'xy/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (xy/org-roam-filter-by-tag "hub")
   :templates
   '(("d" "default" plain "** TODO %?"
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                     "#+title: ${title}
#+category: ${title}
#+filetags: hub
")))))

;; (defun xy/org-roam-create-inbox-entry ()
;;   (interactive)
;;   (org-roam-capture- :node (org-roam-node-create)
;;                      :templates '(("i" "inbox" plain "* %?"
;;                                    :target
;;                                    (file+head "inbox.org" "#+title: Inbox
;; ")))))

;; (defun xy/org-roam-create-new-project ()
;;   (interactive)
;;   ;; Add the project file to the agenda after capture is finished
;;   (add-hook 'org-capture-after-finalize-hook #'xy/org-roam-project-finalize-hook)

;;   ;; Capture the new task, creating the project file if necessary
;;   (org-roam-capture- :node (org-roam-node-read
;;                             nil
;;                             (xy/org-roam-filter-by-tag "PROJECT"))
;;                      :templates '(("p" "PROJECT" plain "** TODO %?"
;;                                    :target
;;                                    (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
;;                                                   "#+title: ${title}
;; #+category: ${title}
;; #+filetags: PROJECT
;; * Tasks
;; :PROPERTIES:
;; :ROAM_EXCLUDE: t
;; :END:
;; " ("Tasks"))))))

(defun xy/org-roam-dailies-create (&optional today)
  "Create the org-roam dailies file for a date."
  (interactive)
  (require 'f)
  (let (;; (head-file "~/org/templates/dailies.org")
        ;; (head-fallback "#+title: %<%Y-%m-%d>\n#+filetags: :PROJECT:dailies:\n* Mind path\n")
        ;; (headstr (f-read-text head-file))
        (org-roam-dailies-capture-templates
         '(("d" "new file" plain "%?"
            :immediate-finish t
            :target (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>
#+filetags: :dailies:
#+setupfile: setup-dailies.org

* Mind path
**Goal! Action! Focus! Joy!**
- Keep my mind flow on track
- Make today counts and valuable

* Timeline
- Log of the day
- Record the time as soon as important things happened
- Use just a few words to describe what happened

** %U Diary was created.

* Archives
- Backup tasks that are DONE today

* Bookmarks
** [[roam:Task Inbox]]
** [[roam:Bookmark Inbox]]
** [[roam:Vocabulary Inbox]]
** [[roam:My playlist]]
** [[roam:Youtube 自媒体]]

* Notes
- Mainly notes on tasks
- { M-x org-agenda RET d }
- Set goal of the day
- Set goals tasks
- Schedule tasks
- Evaluate things
- Make decisions
- Make deadlines
- Change plans
")))))
    (if today
        (org-roam-dailies-goto-today)
      (org-roam-dailies-goto-date))))

(defun xy/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to t to keep the original!
        (org-roam-dailies-capture-templates
          '(("a" "archive" entry "%?"
             :target (file+head+olp "%<%Y-%m-%d>.org"
                                    "#+title: %<%Y-%m-%d>
#+filetags: :dailies:

* Mind path

* Timeline

** %U Diary was created.

* Archives

* Notes
" ("Archives")))))
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
      (org-refile nil nil (list "Archives" today-file nil pos)))))

;; (add-to-list 'org-after-todo-state-change-hook
;;              (lambda ()
;;                (when (equal org-state "DONE")
;;                  (xy/org-roam-copy-todo-to-today))))
;; (xy/org-roam-refresh-agenda-list)


;; Easily Copy an Org-mode URL
;; REF: https://hungyi.net/posts/copy-org-mode-url/
;; https://emacs.stackexchange.com/questions/3981/how-to-copy-links-out-of-org-mode
;; FIXME: not working on the link in a heading
(defun xy/org-retrieve-url-from-point ()
  "Copies the URL from an org link at the point"
  (interactive)
  (let ((plain-url (url-get-url-at-point)))
    (if plain-url
        (progn
          (kill-new plain-url)
          (message (concat "Copied: " plain-url)))
      (let* ((link-info (assoc :link (org-context)))
             (text (when link-info
                     (buffer-substring-no-properties
                      (or (cadr link-info) (point-min))
                      (or (caddr link-info) (point-max))))))
        (if (not text)
            (error "Oops! Point isn't in an org link")
          (string-match org-link-bracket-re text)
          (let ((url (substring text (match-beginning 1) (match-end 1))))
            (kill-new url)
            (message (concat "Copied: " url))))))))


;; Resolve org-roam ID problems
;; REF:
;;   - https://dev.to/devteam/resolving-an-unable-to-resolve-link-error-for-org-mode-in-emacs-2n1f
;;   - https://takeonrules.com/2022/01/11/resolving-an-unable-to-resolve-link-error-for-org-mode-in-emacs/
;;   - https://github.com/org-roam/org-roam/issues/811
(defun xy/refresh-org-id-cache ()
  "Refresh the `org-id' cache."
  (interactive)
  (delete-file org-id-locations-file t) ;; delete org-id cache
  (org-id-update-id-locations (directory-files-recursively
                               ;; org-roam-directory
                               org-directory
                               ".org$\\|.org.gpg$")))
;; NOTE: this is not in use any more.
;;
;; if I only want to rebuild org-id ids, including
;; org-agenda-files and org-roam files.
;; For me, my org-agenda-files reside in org-roam-directory,
;; so I only need to update org-roam ids.
;;
;; (defun xy/rebuild-org-id-locations ()
;;   "Rebuild org id loactions."
;;   (interactive)
;;   (let ((org-id-files (org-roam--list-files org-roam-directory))
;;         org-agenda-files)
;;     (org-id-update-id-locations)))

(defun xy/refresh-org-roam-db ()
  "Refresh `org-roam' database."
  (interactive)
  ;; (delete-file org-roam-db-location t)  ;; delete org-roam db
  (org-roam-db-clear-all) ;; clear org-roam db is enough
  ;; Rebuild org-id-loactions cache (Hash),
  ;; including org-agenda-files and org-roam files.
  ;; For me, my org-agenda-files reside in org-roam-directory,
  ;; so I only need to update org-roam ids.
  ;; (let ((org-id-files (org-roam--list-files org-roam-directory))
  ;;       org-agenda-files)
  ;;   (org-id-update-id-locations))
  ;; Rebuild org-roam files's id cache
  (org-roam-update-org-id-locations)
  (org-roam-db-sync))


;; Convert `attachment:' links to `file:' links
;; REF: https://vxlabs.com/2020/07/25/emacs-lisp-function-convert-attachment-to-file/
(defun xy/convert-attachment-to-file ()
  "Convert [[attachment:..]] to [[file:..][file:..]]"
  (interactive)
  (let ((elem (org-element-context)))
    (if (eq (car elem) 'link)
        (let ((type (org-element-property :type elem)))
          ;; only translate attachment type links
          (when (string= type "attachment")
            ;; translate attachment path to relative filename using org-attach API
            ;; 2020-11-15: org-attach-export-link was removed, so had to rewrite
            (let* ((link-end (org-element-property :end elem))
                   (link-begin (org-element-property :begin elem))
                   ;; :path is everything after attachment:
                   (file (org-element-property :path elem))
                   ;; expand that to the full filename
                   (fullpath (org-attach-expand file))
                   ;; then make it relative to the directory of this org file
                   (current-dir (file-name-directory (or default-directory
                                                         buffer-file-name)))
                   (relpath (file-relative-name fullpath current-dir)))
              ;; delete the existing link
              (delete-region link-begin link-end)
              ;; replace with file: link and file: description
              (insert (format "[[file:%s][file:%s]]" relpath relpath))))))))


;; Fix org-download-edit problem on org-attach `[[attachment:]]' links
(defun xy/org-download-edit ()
  "Open the image at point for editing."
  (interactive)
  (let ((context (org-element-context)))
    (if (not (eq (car-safe context) 'link))
        (user-error "Not on a link")
          (start-process-shell-command
           "org-download-edit"
           "org-download-edit"
           (format org-download-edit-cmd
                   (shell-quote-wildcard-pattern
                    (url-unhex-string
                     (if (string= (plist-get (cadr context) :type) "attachment")
                       (concat (file-name-as-directory
                                (org-attach-dir))
                               (plist-get (cadr context) :path))
                       (plist-get (cadr context) :path)))))))))


;; TODO: Finish generic function of toggle between different parenthesis of a
;; string types. It can be used to transform org timestamps.
;;
;; REF:
;;   - https://github.com/sprig/org-capture-extension
;;   - https://github.com/sprig/org-capture-extension/issues/37
;;
;; (defun transform-square-brackets-to-round-ones(string-to-transform)
;;   "Transforms [ into ( and ] into ), other chars left unchanged."
;;   (concat
;;    (mapcar #'(lambda (c) (if (equal c ?\[)
;;                              ?\(
;;                            (if (equal c ?\])
;;                                ?\)
;;                              c))) string-to-transform)))
;;
;; (defun transform-round-brackets-to-square-ones(string-to-transform)
;;   "Transforms ( into [ and ) into ], other chars left unchanged."
;;   (concat
;;    (mapcar #'(lambda (c) (if (equal c ?\()
;;                              ?\[
;;                            (if (equal c ?\))
;;                                ?\]
;;                              c))) string-to-transform)))
;;
;; Tests:
;;
;; (transform-square-brackets-to-round-ones "[sweat home]")
;; (transform-square-brackets-to-round-ones "sweat home")
;; (transform-square-brackets-to-round-ones "[]sweat home")
;; (transform-square-brackets-to-round-ones "[sweat home")
;; (transform-round-brackets-to-square-ones "(sweat home)")


(defconst mylob "~/org/roam/my_library_of_babel.org")

;;; Load my library-of-babel
(defun xy/load-lob ()
  "Load my Library of Babel for org-mode."
  (interactive)
  (if (file-exists-p mylob)
      (org-babel-lob-ingest mylob)
    (message (concat mylob " NOT exsists!"))))

;; TODO: add (point-to-register) before calling org-roam-node-insert.
;; (defadvice org-roam-node-insert (around advice-org-roam-node-insert activate)
;;   (interactive)
;;   (if (interactive-p)
;;       (progn
;;         (point-to-register)
;;         (call-interactively (ad-get-orig-definition 'org-roam-node-insert)))
;;     ad-do-it))


;; REF: https://www.youtube.com/watch?v=v-jLg1VaYzo
(defun xy/org-jump-to-heading-beginning ()
  "Jump to the beginning of the line of the closest Org heading."
  (interactive)
  (org-back-to-heading)
  (beginning-of-line))


;; REF: https://www.reddit.com/r/emacs/comments/giqtq6/how_to_integrate_orgwebtools_with_orgcapture/
;; (defun xy/website-to-org-entry ()
;;   "Convert clipboard's URL content to org entry."
;;   (interactive)
;;   (require 'org-web-tools)
;;   (org-web-tools-insert-web-page-as-entry (org-get-x-clipboard 'PRIMARY)))

;; The exported HTML files need to be browsed in my favorite web browser.
;;
;; solution 1. config google-chrome to be the browser, which can be different
;; from the `browse-url-default-browser'
;;
;; (browse-url-chrome (concat "file://" (expand-file-name html)))
;;
;; solution 2. use `browse-url-default-browser', which has been overridden in
;; "../browsers/packages.el::(defun browse-url-default-browser"
;;
;; (browse-url (concat "file://" (expand-file-name html)))
;;
(defun xy/browser-url-local (html url)
  "Open the local html file or URL in browser."
  (if (file-exists-p html)
      (browse-url-chrome (concat "file://" (expand-file-name html)))
    (message (concat url " : File does not exist."))))


;; REF: https://pragmaticemacs.wordpress.com/2015/10/02/wrap-text-in-an-org-mode-block/
;; function to wrap blocks of text in org templates
;; e.g. latex or src etc
;;
;; NOTE: you can `mark-paragraph' then call this to wraph a text snippet 
(defun xy/wrap-region-with-org-begin ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))


;; REF: https://github.com/novoid/dot-emacs/blob/master/config.org
(defun xy/org-attach-insert (&optional in-emacs)
  "Insert attachment from list."
  (interactive "P")
  (let ((attach-dir (org-attach-dir)))
    (if attach-dir
    (let* ((file (pcase (org-attach-file-list attach-dir)
               (`(,file) file)
               (files (completing-read "Insert attachment: "
                           (mapcar #'list files) nil t))))
           (path (expand-file-name file attach-dir))
               (desc (file-name-nondirectory path)))
          (let ((initial-input
             (cond
              ((not org-link-make-description-function) desc)
              (t (condition-case nil
                 (funcall org-link-make-description-function link desc)
               (error
                (message "Can't get link description from %S"
                     (symbol-name org-link-make-description-function))
                (sit-for 2)
                nil))))))
        (setq desc (if (called-interactively-p 'any)
                   (read-string "Description: " initial-input)
                 initial-input))
            (org-insert-link nil path (concat "attachment:" desc))))
      (error "No attachment directory exist"))))
;; (define-key org-mode-map (kbd "C-c o i") #'org-attach-insert)


(defun xy/org-roam-create-node-window-below ()
  "Create a node file in a new window below the current window."
  (interactive)
  (split-window-below-and-focus)
  (org-roam-node-find))


(defun xy/org-roam-create-node-window-right ()
  "Create a node file in a new window on the right of the current window."
  (interactive)
  (split-window-right-and-focus)
  (org-roam-node-find))

;; Use indirect buffer as `org-edit-special' command.
;; REF:
;;   - https://emacs.stackexchange.com/questions/12180/why-use-indirect-buffers
;;   - `clone-indirect-buffer': "simple.el#defun clone-indirect-buffer"
(defun xy/create-indirect-buffer-on-region (start end &optional newname)
  "Edit the current region in another indirect buffer.
    Prompt for a major mode to activate."
  (interactive "r")
  (setq newname (or newname (buffer-name)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (let ((buffer-name (generate-new-buffer-name newname))
        (mode (intern
               (completing-read
                "Mode: "
                (mapcar (lambda (e)
                          (list (symbol-name e)))
                        (apropos-internal "-mode$" 'commandp))
                nil t))))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))

(defun xy/adapt-org-config (&optional frame)
  "Adapt org to work in terminal or graphical environment."
  (interactive)
  (setq system-time-locale "C") ;; use standard time format.
  (or frame (setq frame (selected-frame)))
  (if (display-graphic-p frame)
      (progn
        (when (eq major-mode 'org-mode)
          (setq org-file-apps
                '(("\\.mm\\'" . default)
                  ("\\.x?html?\\'" . xy/browser-url-local) ;; use favorite browser to view HTML
                  ("\\.pdf\\'" . emacs)
                  ("\\.png\\'" . emacs)
                  ("\\.jpg\\'" . emacs)
                  ("\\.jpeg\\'" . emacs)
                  ("\\.bmp\\'" . emacs)
                  ("\\.svg\\'" . emacs)
                  ("\\.gif\\'" . emacs)
                  (directory . emacs)
                  (auto-mode . emacs)))

          ;; Create new frame for indirect buffer
          (setq org-indirect-buffer-display 'dedicated-frame)

          ;; Hook was added in org layer,
          ;;
          ;; <find-function-other-window 'org/init-org-modern>
          ;;
          ;; but I removed in org-extra layer
          ;;
          ;; <find-function-other-window 'org-extra/post-init-org-modern>
          ;; (when (featurep 'org-modern) (org-modern-mode 1))
          ;; (global-org-modern-mode 1)

          (message "Adapt org config for graphical frame."))

      (progn
        (setq org-file-apps
              '(("\\.mm\\'" . default)
                ("\\.x?html?\\'" . xy/browser-url-local)
                ("\\.pdf\\'" . system)
                ("\\.png\\'" . "swayimg %s")
                ("\\.jpg\\'" . "swayimg %s")
                ("\\.jpeg\\'" . "swayimg %s")
                ("\\.bmp\\'" . "swayimg %s")
                ("\\.svg\\'" . "swayimg %s")
                ("\\.gif\\'" . "swayimg %s")
                ;; ("\\.gif\\'" . "pixelhopper %s")
                (directory . emacs)
                (auto-mode . emacs)))

        ;; Create new frame for indirect buffer
        (setq org-indirect-buffer-display 'other-window)

        ;; FIXME: table looks awful in terminal
        ;; hook is not good.
        ;; (when (featurep 'org-modern)
        ;;   (add-hook 'org-mode-hook
        ;;             (lambda () (org-modern-mode -1))))
        ;; (when (featurep 'org-modern) (org-modern-mode -1))
        ;; (global-modern-mode -1)

        (message "Adapt org config for terminal frame.")))

    ;; Turn off windmove-mode which overrides timestamp keys: S-<up>/<down>
    (when (featurep 'windmove) (windmove-mode -1))

    ;; Restart org-mode
    ;; (when (featurep 'org) (org-mode-restart))
  ))

;; -- org faces ------------------------------------------------------------------------
;; (defun xy/org-faces-config ()
;;   "Setup org-mode faces."
;;   (require 'org-faces)
;;   (set-face-attribute 'org-default nil :inherit 'variable-pitch)
;;   (set-face-attribute 'org-document-title nil :inherit '(variable-pitch bold))
;;   (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-block-begin-line nil :inherit '(fixed-pitch bold) :overline nil :underline t :extend t)
;;   (set-face-attribute 'org-block-end-line nil :inherit '(fixed-pitch bold) :overline t :underline nil :extend t)
;;   (set-face-attribute 'org-table-header nil :inherit '(fixed-pitch bold))
;;   (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-code nil :inherit 'fixed-pitch :extend t)
;;   (set-face-attribute 'org-quote nil :inherit '(variable-pitch bold))
;;   (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-date nil :inherit '(fixed-pitch bold))
;;   (set-face-attribute 'org-drawer nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-sexp-date nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-clock-overlay nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-date-selected nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-document-info nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
;;   ;; GUI
;;   ;; set block faces
;;   ;; REF: https://stackoverflow.com/questions/44811679/orgmode-change-code-block-background-color
;;   ;;
;;   ;; NOTE: Set `:background' to "unspecified-bg" if you want an
;;   ;; transparent background
;;   ;; (custom-set-faces
;;   ;;  '(org-block-begin-line
;;   ;;    ((t (:background "unspecified-bg"
;;   ;;         :family "FiraCode Nerd Font Mono"
;;   ;;         :weight extra-bold
;;   ;;         :height 160
;;   ;;         :overline nil
;;   ;;         :underline t
;;   ;;         :extend t))))
;;   ;;  '(org-block
;;   ;;    ((t (:background "unspecified-bg"
;;   ;;         :family "FiraCode Nerd Font Mono"
;;   ;;         :width condensed
;;   ;;         :height 120
;;   ;;         :overline nil
;;   ;;         :underline nil
;;   ;;         :extend t))))
;;   ;;  '(org-block-end-line
;;   ;;    ((t (:background "unspecified-bg"
;;   ;;         :family "FiraCode Nerd Font Mono"
;;   ;;         :weight extra-bold
;;   ;;         :height 160
;;   ;;         :overline t
;;   ;;         :underline nil
;;   ;;         :extend t)))))
;;   ;;
;;   ;; Terminal
;;   ;; set block faces
;;   ;; REF: https://stack overflow.com/questions/44811679/orgmode-change-code-block-background-color
;;   ;; (custom-set-faces
;;   ;;  '(org-block-begin-line
;;   ;;    ((t (:background "unspecified-bg" :weight bold
;;   ;;                     :underline nil :overline nil :extend nil))))
;;   ;;  '(org-block
;;   ;;    ((t (:background "unspecified-bg"
;;   ;;                     :underline nil :overline nil :extend t))))
;;   ;;  '(org-block-end-line
;;   ;;    ((t (:background "unspecified-bg" :weight bold
;;   ;;                     :underline nil :overline nil :extend nil)))))
;;   )
