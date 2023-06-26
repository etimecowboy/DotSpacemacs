;;; config.el --- Org-extra configuration File for Spacemacs
;; Time-stamp: <2023-06-21 Wed 17:32 by xin on tufg>
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
