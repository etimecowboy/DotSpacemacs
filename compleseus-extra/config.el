;;; config.el --- Compleseus-extra configuration File for Spacemacs
;; Time-stamp: <2022-11-23 Wed 03:49 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

(global-set-key (kbd "M-O") 'embark-act-noquit)
(global-set-key (kbd "M-L") 'embark-live)

(add-to-list 'display-buffer-alist
             '("\\*Embark" display-buffer-same-window)
             ;; '("Embark\\ Live" display-buffer-pop-up-frame)
             ;; '("Embark\\ Export" display-buffer-at-bottom)
             ;; '("^\\*Embark\\ Live.*\\*$" display-buffer-pop-up-frame)
             ;; '("^\\*Embark\\ Export.*\\*$" display-buffer-at-bottom)
             ;; '("^\\*Embark.*\\*$" display-buffer-at-bottom)
              )
(setq embark-quit-after-action nil)
(setq consult-preview-key (list :debounce 0.75 'any))
                          ;; nil)
                          ;; (kbd "M-."))

;; (add-to-list 'embark-keymap-alist '(consult-org-heading . embark-org-heading-map))
;; (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map))
