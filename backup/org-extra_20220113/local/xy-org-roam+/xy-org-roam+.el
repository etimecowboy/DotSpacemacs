;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; xy-org-roam+.el --- My tricks for org-roam

;; Copyright (C) 2022 etimecowboy <etimecowboy@gmail.com>

;; Author: etimecowboy
;; Keywords: org org-roam
;; Package-Version:

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provide extra functions for smoother org-roam workflow

;;; Code:


(require 'org)
(require 'org-roam)

;; REF: https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(defun xy/org-roam-node-insert-immediate (arg &rest args)
  "Create a new node and insert a link in the current document without opening the new node's buffer."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

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
  (setq org-agenda-files (xy/org-roam-list-notes-by-tag "PROJECT")))

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
   '(("p" "project" plain "* Goals

%?

* Tasks

** TODO Add initial tasks

* Dates

"
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+category: ${title}
#+filetags: PROJECT")
      :unnarrowed t))))

(defun xy/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :target
                                   (file+head "inbox.org" "#+title: Inbox
")))))

(defun xy/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'xy/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (xy/org-roam-filter-by-tag "PROJECT"))
                     :templates '(("p" "PROJECT" plain "** TODO %?"
                                   :target
                                   (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                  "#+title: ${title}
#+category: ${title}
#+filetags: PROJECT" ("Tasks"))))))

(defun xy/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :target (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
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
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(provide 'xy-org-roam+)

;;; xy-org-roam+.el ends here
