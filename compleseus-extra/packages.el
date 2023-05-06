;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- compleseus-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-05-06 Sat 03:31 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq compleseus-extra-packages
      '(
        consult-dir
        (eli-image :location local) ;; only works in minibuffer, not in vertico-posframe-mode
        ;; consult-project-extra ;; not as good as consult-projectile
        consult-projectile
        ;; consult-flycheck
        ;; consult-flyspell
        ;; consult-company ;; remove all company staff
        consult-org-roam
        yasnippet
        yasnippet-snippets
	      ;; vertico-quick
	      ;; vertico-repeat
	      vertico-posframe
        ))

(defun compleseus-extra/init-consult-dir ()
  (use-package consult-dir
    :ensure t
    :bind (("C-x C-d" . consult-dir)
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file))))

(defun compleseus-extra/init-eli-image ()
  (use-package eli-image
    :commands (eli-select-images)
    :bind (("M-I" . xy/eli-select-images))
    ))

;; (defun compleseus-extra/init-consult-project-extra ()
;;   (use-package consult-project-extra))

(defun compleseus-extra/init-consult-projectile ()
  (use-package consult-projectile
    :bind (
           ("M-s p" . consult-projectile)
           )))

;; remove all company staff
;; (defun compleseus-extra/init-consult-company ()
;;   (use-package consult-company
;;     :bind (
;;            ("M-s c" . consult-company)
;;            )
;;     ))

;; load consult-org-roam
(defun compleseus-extra/init-consult-org-roam ()
  (spacemacs|use-package-add-hook org-roam
    :post-config (require 'consult-org-roam))
  (use-package consult-org-roam
    :after org-roam
    :custom
    ;; Use `ripgrep' for searching with `consult-org-roam-search'
    (consult-org-roam-grep-func #'consult-ripgrep)
    ;; Configure a custom narrow key for `consult-buffer'
    (consult-org-roam-buffer-narrow-key ?r)
    ;; Display org-roam buffers right after non-org-roam buffers
    ;; in consult-buffer (and not down at the bottom)
    (consult-org-roam-buffer-after-buffers t)
    :config
    (consult-org-roam-mode 1)
    ;; Eventually suppress previewing for certain functions
    (consult-customize
     consult-org-roam-forward-links
     :preview-key (kbd "M-."))
    (spacemacs|diminish consult-org-roam-mode)
    :bind
    ("M-s n" . consult-org-roam-file-find)
    ("M-s b" . consult-org-roam-backlinks)
    ("M-s s" . consult-org-roam-search)
    ("M-s F" . consult-org-roam-forward-links)
    ("M-s a" . consult-org-agenda)
    ("M-s h" . consult-org-heading)
    ("M-s I" . consult-info)
    ("M-s M" . consult-man)
    ("M-s y" . consult-yasnippet)
    ("M-y"   . consult-yank-replace)
    ("M-s i" . org-roam-node-insert)
    ))

(defun compleseus-extra/init-yasnippet ()
  (use-package yasnippet
    :ensure t
    :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
    :init
    (defvar yas-snippet-dirs nil)
    (setq auto-completion-private-snippets-directory "/home/xin/src/spacemacs/private/snippets")
    (add-to-list 'yas-snippet-dirs 'auto-completion-private-snippets-directory)
    :config
    (yas-global-mode 1)
    (spacemacs|diminish yas-minor-mode " ⓨ" " y")
    ))

(defun compleseus-extra/init-yasnippet-snippets ())

;; (defun compleseus-extra/init-vertico-quick ()
;;   (use-package vertico-quick))

;; (defun compleseus-extra/init-vertico-repeat ()
;;   (use-package vertico-repeat))

(defun compleseus-extra/init-vertico-posframe ()
  (use-package vertico-posframe
    :ensure t
    :after (vertico posframe)
    :config
    (setq vertico-posframe-fallback-mode 'vertico-buffer-mode
          vertico-posframe-poshandler 'posframe-poshandler-point-frame-center)
    (vertico-posframe-mode t)
    ))
