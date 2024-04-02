;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- demo layer packages file for Spacemacs.
;; Time-stamp: <2024-03-29 Fri 03:25 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq demo-packages
      '(
        command-log-mode
        gif-screencast
        keycast
        (screenshot :location
                    (recipe :fetcher github
                            :repo "tecosaur/screenshot"))
        org-tree-slide ;; required by demo-it
	;; FIXME: error when `fancy-narrow' is enabled
        ;; fancy-narrow ;; required by demo-it
        demo-it
        ))

;; load command-log-mode
(defun demo/init-command-log-mode ()
  (use-package command-log-mode))

;; load gif-screencast
(defun demo/init-gif-screencast ()
  (use-package gif-screencast
    :bind
    (:map gif-screencast-mode-map
          ("<f7>" . gif-screencast-start-or-stop)
          ("<f8>" . gif-screencast-toggle-pause)
          ("<f9>" . gif-screencast-stop))))

;; load keycast
(defun demo/init-keycast ()
  (use-package keycast
    :config
    (setq keycast-tab-bar-location  'replace
          keycast-tab-bar-format "%k%2s%c%R")))

;; load screenshot
(defun demo/init-screenshot ()
  (use-package screenshot
    ;; :config
    ;; (setq screenshot-border-width 0
    ;;       screenshot-line-numbers-p t
    ;;       screenshot-relative-line-numbers-p t)
    ))

;; load org-tree-slide
(defun demo/init-org-tree-slide ()
  (use-package org-tree-slide
    :bind
    (:map org-tree-slide-mode-map
          ("<f7>"  . org-tree-slide-move-previous-tree)
          ("<f8>" . org-tree-slide-move-next-tree)
          ("<f9>" . org-tree-slide-content))
    :custom
    (setq org-tree-slide-skip-outline-level 4)
    (org-tree-slide-narrowing-control-profile)
    (setq org-tree-slide-skip-done nil)))

;; FIXME: error in fancy-narrow
;; load fancy-narrow
(defun demo/init-fancy-narrow ()
  (use-package fancy-narrow
    :after org
    :init
    (require 'org-macs)
    (setq org-speed-commands
          (cons '("S" . xy/toggle-org-fancy-narrow-to-subtree)
                org-speed-commands))
    :config
    (defmacro org-with-limited-levels (&rest body)
      "Execute BODY with limited number of outline levels."
      (declare (debug (body)))
      `(progn
         (defvar org-called-with-limited-levels)
         (defvar org-outline-regexp)
         (defvar outline-regexp)
         (defvar org-outline-regexp-bol)
         (let* ((org-called-with-limited-levels t)
                (org-outline-regexp (org-get-limited-outline-regexp))
                (outline-regexp org-outline-regexp)
                (org-outline-regexp-bol (concat "^" org-outline-regexp)))
           ,@body)))
    
    (defun xy/toggle-org-fancy-narrow-to-subtree ()
      (interactive)
      (if (fancy-narrow-active-p)
          (fancy-widen)
        (org-fancy-narrow-to-subtree)))
  
    ;; Bug Fix: `fancy-narrow' package uses macro `org-with-limited-levels' of
    ;; `org-macs' package, but did not require it at the beginning of
    ;; `fancy-narrow.el'
    ;; (spacemacs|use-package-add-hook org
    ;;   :post-config
    ;;   (require 'org-macs)
    ;;   (require 'fancy-narrow)
    ;;   ;; NOTE: `fancy-narrow' overrides the default `narrow-to-*' and etc.
    ;;   ;; functions. I will never use `fancy-narrow-mode'. Instead, I put all
    ;;   ;; `fancy-narrow' function to "keybindings.el"
    ;;   (setq org-speed-commands
    ;;         (cons '("S" . xy/toggle-org-fancy-narrow-to-subtree)
    ;;               org-speed-commands)))
    ;;   :config
      ;; (defmacro org-with-limited-levels (&rest body)
      ;;   "Execute BODY with limited number of outline levels."
      ;;   (declare (debug (body)))
      ;;   `(progn
      ;;      (defvar org-called-with-limited-levels)
      ;;      (defvar org-outline-regexp)
      ;;      (defvar outline-regexp)
      ;;      (defvar org-outline-regexp-bol)
      ;;      (let* ((org-called-with-limited-levels t)
      ;;             (org-outline-regexp (org-get-limited-outline-regexp))
      ;;             (outline-regexp org-outline-regexp)
      ;;             (org-outline-regexp-bol (concat "^" org-outline-regexp)))
      ;;        ,@body)))

    ;; (defun xy/toggle-org-fancy-narrow-to-subtree ()
    ;;   (interactive)
    ;;   (if (fancy-narrow-active-p)
    ;;       (fancy-widen)
    ;;     (org-fancy-narrow-to-subtree)))
    ))

;; load demo-it
(defun demo/init-demo-it ()
  (use-package demo-it
    :config
    (setq demo-it--shell-or-eshell :shell
          demo-it--text-scale 4)))
