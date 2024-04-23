;;; config.el --- Org-extra configuration File for Spacemacs
;; Time-stamp: <2024-04-18 Thu 06:45 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; (defvar org-recover-vertico-posframe-mode-p nil)

;; (advice-add 'org-capture
;;             :before (lambda ()
;;                       (if (featurep 'vertico-posframe)
;;                           (when vertico-posframe-mode
;;                             (vertico-posframe-mode -1)
;;                             (setq org-recover-vertico-posframe-mode-p t)))))

;; (advice-add 'org-roam-capture
;;             :before (lambda ()
;;                       (if (featurep 'vertico-posframe)
;;                           (when vertico-posframe-mode
;;                             (vertico-posframe-mode -1)
;;                             (setq org-recover-vertico-posframe-mode-p t)))))

;; (add-hook 'minibuffer-exit-hook
;;           (lambda ()
;;             (posframe-delete-all)
;;             (if (featurep 'vertico-posframe)
;;                 (when org-recover-vertico-posframe-mode-p
;;                   (vertico-posframe-mode 1)
;;                   (setq org-recover-vertico-posframe-mode-p nil)))))

;; Variables

;; (defvar xy:org-fixed-width-font "Noto Mono CJK SC"
;;   "The font to use for monospaced (fixed width) text in org-mode.")

;; (defvar xy:org-fixed-serif-font "Noto Serif CJK SC"
;;   "The serif font to use for org-mode.")

;; (defvar xy:org-variable-width-font "Noto Sans CJK SC"
;;   "The font to use for variable-pitch (document) text in org-mode.")

;; (defvar xy:org-variable-text-height 1.1
;;   "The height scale for variable-pitch-text face in org-mode.")
