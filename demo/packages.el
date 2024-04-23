;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- demo layer packages file for Spacemacs.
;; Time-stamp: <2024-04-22 Mon 14:29 by xin on tufg>
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
        writeroom-mode
        mixed-pitch
        (fixed-pitch :location
                     (recipe :fetcher github
                             :repo "cstby/fixed-pitch-mode"))
        focus
        olivetti
        ))

;; load command-log-mode
(defun demo/init-command-log-mode ()
  (use-package command-log-mode
    :defer t
    :commands
    (global-command-log-mode
     clm/toggle-command-log-buffer
     clm/command-log-clear
     clm/save-command-log)
    :custom
    (;; (global-command-log-mode)
     ;; (command-log-mode-auto-show t)
     (command-log-mode-is-global t)
     (command-log-mode-window-size 55)
     (command-log-mode-window-font-size 1))
    ;; :config
    ;; (setq command-log-mode-window-font-size 1)
    ;; (setq command-log-mode-window-size 40)
    ))

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
    :hook
    ((org-tree-slide-play . xy/org-tree-slide-play-setup)
     (org-tree-slide-stop . xy/org-tree-slide-stop-setup))
    :bind
    (:map org-tree-slide-mode-map
          ("C-<" . org-tree-slide-move-previous-tree)
          ("C->" . org-tree-slide-move-next-tree)
          ("C-," . org-tree-slide-content)
          ("C-." . org-tree-slide-content))
    :custom
    (org-image-actual-width nil)
    (org-tree-slide-activate-message "Presentation started!")
    (org-tree-slide-deactivate-message "Presentation finished!")
    (org-tree-slide-skip-outline-level 4)
    (org-tree-slide-skip-done t)
    (org-tree-slide-breadcrumbs " ⮞ ")
    :config
    (defun xy/org-tree-slide-play-setup ()
      "Prepare for the org-tree-slide demo."
      (require 'writeroom-mode)
      (require 'command-log-mode)
      (global-hl-line-mode -1)
      (setq-default org-hide-emphasis-markers t)
      (setq-local org-hide-emphasis-markers t)
      (org-redisplay-inline-images)
      (writeroom-mode 1)
      ;; (text-scale-set 4)
      ;; (visual-fill-column-mode -1)
      (global-command-log-mode 1)
      ;; NOTE: don't open the log buffer by default
      ;; (clm/open-command-log-buffer)
      )

    (defun xy/org-tree-slide-stop-setup ()
      "Endup the org-tree-slide demo."
      (require 'writeroom-mode)
      (require 'command-log-mode)
      (global-hl-line-mode 1)
      (setq-default org-hide-emphasis-markers nil)
      (setq-local org-hide-emphasis-markers nil)
      (clm/close-command-log-buffer)
      ;; NOTE: close the log buffer no matter it exists or not
      (global-command-log-mode -1)
      (writeroom-mode -1)
      (xy/set-hide-emphasis-markers -1))

    ;; default profile
    ;; (org-tree-slide-simple-profile)
    (org-tree-slide-presentation-profile)
    ;; (org-tree-slide-narrowing-control-profile)
    ))

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

;; writeroom extra config
(defun demo/post-init-writeroom-mode ()
  (setq writeroom-width 90
        writeroom-extra-line-spacing 0.2
        writeroom-header-line nil
        writeroom-border-width 30
        writeroom-bottom-divider-width 5
        writeroom-restore-window-config t
        writeroom-global-effects
        '(;; xy/set-hide-emphasis-markers
          writeroom-set-fullscreen
          writeroom-set-alpha
          writeroom-set-menu-bar-lines
          writeroom-set-tool-bar-lines
          writeroom-set-vertical-scroll-bars
          writeroom-set-internal-border-width
          writeroom-set-bottom-divider-width
          xy/set-face-remapping-alist
          xy/set-mixed-pitch
          xy/set-buffer-text-scale
          )))

;; load mixed-pitch
(defun demo/init-mixed-pitch ()
  (use-package mixed-pitch
    :ensure t
    :init
    (if xy:display-mixed-pitch
        (progn
          (xy/set-pitch-faces (xy:fixed-pitch-font
                               xy:fixed-pitch-serif-font
                               xy:variable-pitch-font
                               xy:variable-pitch-text-height))
          (add-hook 'org-mode-hook #'mixed-pitch-mode)
          (add-hook 'text-mode-hook #'mixed-pitch-mode))
      (progn
        (remove-hook 'org-mode-hook #'mixed-pitch-mode)
        (remove-hook 'text-mode-hook #'mixed-pitch-mode)))
    :custom
    (mixed-pitch-variable-pitch-cursor 'bar)
    (mixed-pitch-set-height t)
    :config
    ;; (setq-default cursor-type 'bar)
    (add-list-to-list 'mixed-pitch-fixed-pitch-faces
                      '(org-clock-overlay org-data org-date-selected
                        org-sexp-data org-table-header))
    (delq nil (delete-dups mixed-pitch-fixed-pitch-faces))
    ))

;; load fixed-pitch
(defun demo/init-fixed-pitch ()
  (use-package fixed-pitch
    :defer t))

;; load focus.el
(defun demo/init-focus ()
  (use-package focus
    :defer t))

;; load olivetti
(defun demo/init-olivetti ()
  (use-package olivetti
    :defer t))
