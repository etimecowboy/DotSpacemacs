;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- demo layer packages file for Spacemacs.
;; Time-stamp: <2024-04-29 Mon 06:18:45 GMT by xin on tufg>
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
        focus
        ;; olivetti
        visual-fill-column
        adaptive-wrap
        hl-line
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
    ;; (global-command-log-mode)
    ;; (command-log-mode-auto-show t)
    (command-log-mode-is-global t)
    (command-log-mode-window-size 55)
    (command-log-mode-window-font-size 1)
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
    ;; default profile
    ;; (org-tree-slide-simple-profile)
    (org-tree-slide-presentation-profile)
    ;; (org-tree-slide-narrowing-control-profile)
    ))

;; FIXME: error in fancy-narrow
;; load fancy-narrow
;; (defun demo/init-fancy-narrow ()
;;   (use-package fancy-narrow
;;     :after org
;;     :init
;;     (require 'org-macs)
;;     (setq org-speed-commands
;;           (cons '("S" . xy/toggle-org-fancy-narrow-to-subtree)
;;                 org-speed-commands))
;;     :config
;;     (defmacro org-with-limited-levels (&rest body)
;;       "Execute BODY with limited number of outline levels."
;;       (declare (debug (body)))
;;       `(progn
;;          (defvar org-called-with-limited-levels)
;;          (defvar org-outline-regexp)
;;          (defvar outline-regexp)
;;          (defvar org-outline-regexp-bol)
;;          (let* ((org-called-with-limited-levels t)
;;                 (org-outline-regexp (org-get-limited-outline-regexp))
;;                 (outline-regexp org-outline-regexp)
;;                 (org-outline-regexp-bol (concat "^" org-outline-regexp)))
;;            ,@body)))

;;     (defun xy/toggle-org-fancy-narrow-to-subtree ()
;;       (interactive)
;;       (if (fancy-narrow-active-p)
;;           (fancy-widen)
;;         (org-fancy-narrow-to-subtree)))

;;     ;; Bug Fix: `fancy-narrow' package uses macro `org-with-limited-levels' of
;;     ;; `org-macs' package, but did not require it at the beginning of
;;     ;; `fancy-narrow.el'
;;     ;; (spacemacs|use-package-add-hook org
;;     ;;   :post-config
;;     ;;   (require 'org-macs)
;;     ;;   (require 'fancy-narrow)
;;     ;;   ;; NOTE: `fancy-narrow' overrides the default `narrow-to-*' and etc.
;;     ;;   ;; functions. I will never use `fancy-narrow-mode'. Instead, I put all
;;     ;;   ;; `fancy-narrow' function to "keybindings.el"
;;     ;;   (setq org-speed-commands
;;     ;;         (cons '("S" . xy/toggle-org-fancy-narrow-to-subtree)
;;     ;;               org-speed-commands)))
;;     ;;   :config
;;       ;; (defmacro org-with-limited-levels (&rest body)
;;       ;;   "Execute BODY with limited number of outline levels."
;;       ;;   (declare (debug (body)))
;;       ;;   `(progn
;;       ;;      (defvar org-called-with-limited-levels)
;;       ;;      (defvar org-outline-regexp)
;;       ;;      (defvar outline-regexp)
;;       ;;      (defvar org-outline-regexp-bol)
;;       ;;      (let* ((org-called-with-limited-levels t)
;;       ;;             (org-outline-regexp (org-get-limited-outline-regexp))
;;       ;;             (outline-regexp org-outline-regexp)
;;       ;;             (org-outline-regexp-bol (concat "^" org-outline-regexp)))
;;       ;;        ,@body)))

;;     ;; (defun xy/toggle-org-fancy-narrow-to-subtree ()
;;     ;;   (interactive)
;;     ;;   (if (fancy-narrow-active-p)
;;     ;;       (fancy-widen)
;;     ;;     (org-fancy-narrow-to-subtree)))
;;     ))

;; load demo-it
(defun demo/init-demo-it ()
  (use-package demo-it
    :config
    (setq demo-it--shell-or-eshell :shell
          demo-it--text-scale 4)))

;; writeroom extra config
(defun demo/post-init-writeroom-mode ()
  (setq writeroom-width 120
        writeroom-extra-line-spacing 0.25
        writeroom-header-line nil
        writeroom-border-width 30
        writeroom-bottom-divider-width 5
        writeroom-restore-window-config t)
  (setq writeroom-global-effects '(;; xy/set-hide-emphasis-markers ;; only for demo, no good for writing
                                   writeroom-set-fullscreen
                                   writeroom-set-alpha
                                   writeroom-set-menu-bar-lines
                                   writeroom-set-tool-bar-lines
                                   writeroom-set-vertical-scroll-bars
                                   writeroom-set-internal-border-width
                                   writeroom-set-bottom-divider-width
                                   ;; xy/set-face-remapping-alist
                                   ;; xy/set-mixed-pitch
                                   xy/set-buffer-text-scale
                                   xy/set-typographic-editing
                                   )))

;; load focus.el
(defun demo/init-focus ()
  (use-package focus
    :init
    ;; HACK: Override the `focus' APIs to deal with org-mode specifically.
    ;;
    ;; Since org-9.7 `thingatpt.el' has stopped supporting `org-element.el', and
    ;; `org-elemnt' was not a option of `thing-at-point-provider-alist' any
    ;; longer.
    (defun xy/focus-next-thing (&optional n)
      "Hacked `focus-next-thing'."
      (interactive "p")
      (or n (setq n 1))
      (if (eq major-mode 'org-mode)
          (dotimes (i n)
            (org-forward-element))
        (focus-next-thing n)))

    (defun xy/focus-prev-thing (&optional n)
      "Hacked `focus-prev-thing'."
      (interactive "p")
      (or n (setq n 1))
      (if (eq major-mode 'org-mode)
          (dotimes (i n)
            (org-backward-element))
        (focus-prev-thing n)))
    :bind
    (:map focus-read-only-mode-map
          ("n" . xy/focus-next-thing)
          ("p" . xy/focus-prev-thing)
          ("j" . xy/focus-next-thing)
          ("k" . xy/focus-prev-thing)
          ("m" . focus-pin)
          ("u" . focus-unpin)
          ("." . focus-change-thing)
          ("N" . org-tree-slide-move-next-tree)
          ("P" . org-tree-slide-move-previous-tree)
          ("l" . org-tree-slide-move-next-tree)
          ("h" . org-tree-slide-move-previous-tree)
          ("c" . org-tree-slide-content))
    :custom
    (focus-mode-to-thing '((prog-mode . defun)
                           (emacs-lisp-mode . list)
                           (text-mode . paragraph)
                           (org-mode . org-element)))
    :config
    ;; (setq focus-mode-to-thing '((prog-mode . defun)
    ;;                             (emacs-lisp-mode . list)
    ;;                             (text-mode . paragraph)
    ;;                             (org-mode . org-element)))
    ;; (define-key focus-read-only-mode-map (kbd "n") 'xy/focus-next-thing)
    ;; (define-key focus-read-only-mode-map (kbd "p") 'xy/focus-prev-thing)
    ;; (define-key focus-read-only-mode-map (kbd "m") 'focus-pin)
    ;; (define-key focus-read-only-mode-map (kbd "u") 'focus-unpin)
    ;; (define-key focus-read-only-mode-map (kbd "c") 'focus-change-thing)
    ))

;; ;; load olivetti
;; (defun demo/init-olivetti ()
;;   (use-package olivetti
;;     :defer t))

(defun demo/init-visual-fill-column ()
  (use-package visual-fill-column
    :ensure t
    :hook
    (visual-line-mode . visual-fill-column-mode)
    (org-mode . visual-line-fill-column-mode)
    (text-mode . visual-line-fill-column-mode)
    :custom
    (visual-fill-column-width 120)
    :config
    (setq visual-fill-column-enable-sensible-window-split t)
    (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
    ))

(defun demo/init-adaptive-wrap ()
  (use-package adaptive-wrap
    :ensure t
    :hook
    (visual-line-mode . adaptive-wrap-prefix-mode)
    (visual-line-fill-column-mode . adaptive-wrap-prefix-mode)
    ))

(defun demo/init-hl-line ()
  (use-package hl-line
    ;; :custom-face
    ;; (hl-line ((t (:extend t :background "#444444"))))
    ))

