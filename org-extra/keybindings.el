;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- org-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-02-23 Thu 07:07 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs/declare-prefix "aoF" "org-fc")
(spacemacs/declare-prefix "aorR" "org-roam-ref")

(spacemacs/declare-prefix-for-mode 'org-mode "F" "org-fc")
(spacemacs/declare-prefix-for-mode 'org-mode "w" "web")
(spacemacs/declare-prefix-for-mode 'org-mode "wi" "insert")
(spacemacs/declare-prefix-for-mode 'org-mode "wa" "archive")
(spacemacs/declare-prefix-for-mode 'org-mode "rR" "org-roam-ref")

(spacemacs/set-leader-keys
  "aoFr" 'org-fc-review
  "aoFR" 'org-fc-review-resume
  "aoFq" 'org-fc-review-quit
  "aoFn" 'org-fc-narrow
  "aoFd" 'org-fc-dashboard
  "aoS"  'org-tree-slide-mode
  "aod"  'org-tree-slide-skip-done-toggle
  "aorRa" 'org-roam-ref-add
  "aorRr" 'org-roam-ref-remove
  "aorRf" 'org-roam-ref-find
  "aordc" 'org-roam-dailies-capture-today
  "aordC" 'org-roam-dailies-capture-date
  "aordO" 'org-roam-dailies-capture-tomorrow
  "aordE" 'org-roam-dailies-capture-yesterday
  "aoru"  'org-roam-ui-mode
  "aorA" 'xy/org-roam-refresh-agenda-list
  "aorP" 'xy/org-roam-find-project
  "aorH" 'xy/org-roam-find-hub
  "aorS" 'xy/refresh-org-id-cache
  "aorL" 'xy/rebuild-org-id-locations
  "Tw" 'writeroom-mode)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "Fr" 'org-fc-review
  "FR" 'org-fc-review-resume
  "Fq" 'org-fc-review-quit
  "Fn" 'org-fc-narrow
  "Fd" 'org-fc-dashboard
  "wr" 'org-web-tools-read-url-as-org
  "wc" 'org-web-tools-convert-links-to-page-entries
  "wil" 'org-web-tools-insert-link-for-url
  "wie" 'org-web-tools-insert-web-page-as-entry
  "waa" 'org-web-tools-archive-attach
  "wav" 'org-web-tools-archive-view
  "rF"  'org-roam-node-find
  "rRa" 'org-roam-ref-add
  "rRr" 'org-roam-ref-remove
  "rRf" 'org-roam-ref-find
  "sR"  'org-roam-refile
  "rdc" 'org-roam-dailies-capture-today
  "rdC" 'org-roam-dailies-capture-date
  "rdO" 'org-roam-dailies-capture-tomorrow
  "rdE" 'org-roam-dailies-capture-yesterday
  "ru"  'org-roam-ui-mode
  "rS" 'xy/refresh-org-id-cache
  "rL" 'xy/rebuild-org-id-locations
  "rE" 'org-roam-extract-subtree
  "I"  'org-roam-node-insert
  "rI" 'xy/org-roam-node-insert-immediate
  "rA" 'xy/org-roam-refresh-agenda-list
  "rP" 'xy/org-roam-find-project
  "rH" 'xy/org-roam-find-hub
  "mu" 'xy/org-retrieve-url-from-point
  "mf" 'xy/convert-attachment-to-file
  "mb" 'org-cycle-list-bullet
  "iDe" 'org-download-edit
  "me" 'xy/org-download-edit
  "iI" 'org-id-get-create
  "iN" 'org-roam-node-insert
  "E"  'org-encrypt-entry
  "D"  'org-decrypt-entry
  "ml" 'xy/load-lob
  "o"  'org-toc-show
  "um" 'org-transclusion-make-from-link
  "uo" 'org-transclusion-open-source)
