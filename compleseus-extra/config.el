;;; config.el --- Compleseus-extra configuration File for Spacemacs
;; Time-stamp: <2023-03-02 Thu 01:29 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:
(add-to-list 'display-buffer-alist
             '("\\*Embark" display-buffer-same-window)
             ;; '("Embark\\ Live" display-buffer-pop-up-frame)
             ;; '("Embark\\ Export" display-buffer-at-bottom)
             ;; '("^\\*Embark\\ Live.*\\*$" display-buffer-pop-up-frame)
             ;; '("^\\*Embark\\ Export.*\\*$" display-buffer-at-bottom)
             ;; '("^\\*Embark.*\\*$" display-buffer-at-bottom)
              )
(setq embark-quit-after-action nil)

;; FIXME: Invalid preview key according to ‘key-valid-p’
;; REF:
;;   - https://github.com/doomemacs/doomemacs/issues/7064
;;   - https://github.com/syl20bnr/spacemacs/pull/15928
;; (setq consult-preview-key (list :debounce 0.75 'any)) ;; nil) ;; (kbd "M-."))
;; (with-eval-after-load "consult"
;;   (consult-customize
;;    consult-theme
;;    :preview-key (list "M-." :debounce 0.5 'any)))

(setq marginalia-separator "  |  ")

(with-eval-after-load "consult-org-roam"
  (spacemacs|diminish consult-org-roam-mode))

;; (with-eval-after-load "embark"
;;   (embark-define-keymap embark-org-heading-map
;;                         "Keymap for Embark heading actions."
;;                         ("i" embark-insert)
;;                         ("b" consult-heading-insert-backlink)
;;                         ("w" embark-copy-as-kill)
;;                         ("q" embark-toggle-quit)
;;                         ("E" embark-export)
;;                         ("S" embark-collect)
;;                         ("L" embark-live)
;;                         ("B" embark-become)
;;                         ("A" embark-act-all)
;;                         ("C-s" embark-isearch)
;;                         ("SPC" mark)
;;                         ("DEL" delete-region))

;;   (embark-define-keymap embark-org-roam-map
;;                         "Keymap for Embark org roam actions."
;;                         ("i" org-roam-node-insert)
;;                         ("s" embark-collect)
;;                         ("b" eli-org-roam-backlinks-node-read))

;;   (add-to-list 'embark-keymap-alist
;;                '(consult-org-heading . embark-org-heading-map))
;;   (add-to-list 'embark-keymap-alist
;;                '(org-roam-node . embark-org-roam-map)))
