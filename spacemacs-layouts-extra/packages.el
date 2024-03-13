;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- spacemacs-layouts-extra layer packages file for Spacemacs.
;; Time-stamp: <2024-03-09 Sat 06:20 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; This layer add additional configurations to spacemacs layouts layer.
;;
;; NOTE: `presp-mode' has been excluded.
;;
;;; Code:

(defconst spacemacs-layouts-extra-packages
  '(
    desktop
    eyebrowse
    eyebrowse-restore
    ))

(defun spacemacs-layouts-extra/init-desktop ()
  (use-package desktop
    :ensure t
    :init
    (setq desktop-dirname spacemacs-cache-directory)

    :bind
    (("C-c C-w C-s" . xy/desktop-save)
     ("C-c C-w C-f" . xy/desktop-read)
     ("C-c C-w C-l" . desktop-clear)
     ("C-c C-w C-v" . desktop-revert)
     ("C-c C-w C-r" . desktop-remove)
     ("C-c C-w c-d" . desktop-change-dir))

    :config
    (setq desktop-save t
          desktop-auto-save-timeout 60 ;; no auto-save
          desktop-dirname spacemacs-cache-directory
          desktop-load-locked-desktop t
          desktop-restore-frames nil ;; use `eybrowse.el'
          )

    (add-to-list 'desktop-path spacemacs-cache-directory)
    (delq nil (delete-dups desktop-path))

    (add-to-list 'delete-frame-functions #'xy/desktop-save)
    (delq nil (delete-dups delete-frame-functions))

    ;; (setq desktop-buffers-not-to-save
    ;;       (concat "\\("
    ;;               "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
    ;;               "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
    ;;               "\\)$"))
    (add-to-list 'desktop-modes-not-to-save 'dired-mode)
    (add-to-list 'desktop-modes-not-to-save 'Info-mode)
    (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
    (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

    ;; (desktop-save-mode 1)
    ))

(defun spacemacs-layouts-extra/post-init-eyebrowse ()
  (require 'eyebrowse-restore)
  )

(defun spacemacs-layouts-extra/init-eyebrowse-restore ()
  (use-package eyebrowse-restore
    :ensure t
    :after (eyebrowse)
    :bind
    (("C-c C-w l" . eyebrowse-restore)
     ("C-c C-w y" . xy/eyebrowse-save)
     ("C-c C-w Y" . eyebrowse-restore-save-all)
     ("C-c C-w n" . xy/set-frame-name)
     ("C-c C-w f" . select-frame-by-name))

    :init
    ;; (spacemacs/transient-state-register-remove-bindings 'workspace
    ;;   '("l"))
    ;; (spacemacs/transient-state-register-add-bindings 'workspace
    ;;   '(("l" eyebrowse-restore)
    ;;     ("y" xy/eyebrowse-interactive-save)
    ;;     ("Y" eyebrowse-restore-save-all)
    ;;     ("f" select-frame-by-name)))
    (setq eyebrowse-restore-dir (concat spacemacs-cache-directory "eyebrowse-restore"))

 ;;    ;; override hydra menu
 ;;    (spacemacs|transient-state-format-hint workspaces
 ;;      spacemacs--workspaces-ts-full-hint
 ;;      "\n
 ;; Go to^^^^^^                         Actions^^^^
 ;; ─────^^^^^^───────────────────────  ───────^^^^───────────────────────
 ;; [_0_.._9_]^^     nth/new workspace  [_c_/_C_] clone workspace
 ;; [_C-0_.._C-9_]^^ nth/new workspace  [_s_/_S_] single window workspace
 ;; [_<tab>_]^^^^    last workspace     [_d_]^^   close current workspace
 ;; [_n_/_C-l_]^^    next workspace     [_l_]^^   restore workspace
 ;; [_N_/_p_/_C-h_]  prev workspace     [_y_/_Y_] save current/all
 ;; [_w_]^^^^        another workspace  [_R_]^^   rename current workspace
 ;; [_f_]^^^^        select frame       [_?_]^^   toggle help")

    :config
    (add-to-list 'delete-frame-functions #'eyebrowse-restore-save-all)
    (delq nil (delete-dups delete-frame-functions))

    (eyebrowse-restore-mode 1)
    ))
