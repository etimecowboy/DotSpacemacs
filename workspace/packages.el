;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- workspace layer packages file for Spacemacs.
;; Time-stamp: <2024-04-03 Wed 03:54 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; This layer add workspace supports to replace spacemacs layouts layer.
;;
;; NOTE: `presp-mode' `eyebrowser' has been excluded.
;;
;;; Code:

(defconst workspace-packages
  '(
    (burly :location (recipe :fetcher github :repo "alphapapa/burly.el"))
    desktop
    ;; -- Removed --------------------------------------
    ;; eyebrowse
    ;; eyebrowse-restore
    ))

(defun workspace/init-burly ()
  (use-package burly
    :ensure t
    :config
    ;; (add-to-list 'delete-frame-functions #'xy/workspace-save-last)
    ;; (delq nil (delete-dups delete-frame-functions))
    (require 'burly-tabs)
    (burly-tabs-mode 1)
    ))

(defun workspace/pre-init-desktop ()
  (spacemacs|use-package-add-hook desktop
    :post-init
    (setq desktop-save t
          desktop-auto-save-timeout 30
          desktop-load-locked-desktop t
          ;; use `burly.el' to save window config
          desktop-restore-frames nil
          ;; Set in `spacemacs-visual' layer
          desktop-dirname spacemacs-cache-directory
          ;; desktop-restore-eager 20
          )
    :post-config
    ;; (setq desktop-save t
    ;;       desktop-auto-save-timeout 30
    ;;       desktop-load-locked-desktop nil
    ;;       ;; use `burly.el' to save window config
    ;;       desktop-restore-frames nil
    ;;       ;; Set in `spacemacs-visual' layer
    ;;       desktop-dirname spacemacs-cache-directory
    ;;       ;; desktop-restore-eager 20
    ;;       )

    (add-to-list 'desktop-path spacemacs-cache-directory)
    (delq nil (delete-dups desktop-path))

    ;; (add-to-list 'delete-frame-functions #'xy/desktop-save)
    ;; (delq nil (delete-dups delete-frame-functions))

    ;; (setq desktop-buffers-not-to-save
    ;;       (concat "\\("
    ;;               "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
    ;;               "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
    ;;               "\\)$"))

    ;; (setq desktop-buffers-not-to-save
    ;;       (concat "\\("
    ;;               "\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
    ;;               "\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
    ;;               "\\|\\.org_archive\\|\\.bak"
    ;;               "\\)$"))

    (dolist (mode '(dired-mode
                    Info-mode
                    info-lookup-mode
                    fundamental-mode
                    ))
      (add-to-list 'desktop-modes-not-to-save mode))

    (delq nil (delete-dups desktop-modes-not-to-save))

    ;; (add-to-list 'desktop-modes-not-to-save #'dired-mode)
    ;; (add-to-list 'desktop-modes-not-to-save #'Info-mode)
    ;; (add-to-list 'desktop-modes-not-to-save #'info-lookup-mode)
    ;; (add-to-list 'desktop-modes-not-to-save #'fundamental-mode)

    (desktop-save-mode 1)
    ))

;; (defun workspace/post-init-eyebrowse ()
;;   (require 'eyebrowse-restore)
;;   )

;; (defun workspace/init-eyebrowse-restore ()
;;   (use-package eyebrowse-restore
;;     :after (eyebrowse)
;;     :init
;;     ;; (spacemacs/transient-state-register-remove-bindings 'workspace
;;     ;;   '("l"))
;;     ;; (spacemacs/transient-state-register-add-bindings 'workspace
;;     ;;   '(("l" eyebrowse-restore)
;;     ;;     ("y" xy/eyebrowse-interactive-save)
;;     ;;     ("Y" eyebrowse-restore-save-all)
;;     ;;     ("f" select-frame-by-name)))

;;     (setq eyebrowse-restore-dir (concat spacemacs-cache-directory "eyebrowse-restore"))
;;     ;; (add-to-list 'delete-frame-functions #'xy/eyebrowse-restore-save-default)
;;     ;; ;; (add-to-list 'delete-frame-functions #'eyebrowse-restore-save-all)
;;     ;; (delq nil (delete-dups delete-frame-functions))

;;     ;;    ;; override hydra menu
;;     ;;    (spacemacs|transient-state-format-hint workspaces
;;     ;;      spacemacs--workspaces-ts-full-hint
;;     ;;      "\n
;;     ;; Go to^^^^^^                         Actions^^^^
;;     ;; ─────^^^^^^───────────────────────  ───────^^^^───────────────────────
;;     ;; [_0_.._9_]^^     nth/new workspace  [_c_/_C_] clone workspace
;;     ;; [_C-0_.._C-9_]^^ nth/new workspace  [_s_/_S_] single window workspace
;;     ;; [_<tab>_]^^^^    last workspace     [_d_]^^   close current workspace
;;     ;; [_n_/_C-l_]^^    next workspace     [_l_]^^   restore workspace
;;     ;; [_N_/_p_/_C-h_]  prev workspace     [_y_/_Y_] save current/all
;;     ;; [_w_]^^^^        another workspace  [_R_]^^   rename current workspace
;;     ;; [_f_]^^^^        select frame       [_?_]^^   toggle help")

;;     :config
;;     (setq eyebrowse-restore-dir (concat spacemacs-cache-directory "eyebrowse-restore"))
;;     ;; (add-to-list 'delete-frame-functions #'xy/eyebrowse-restore-save-default)
;;     ;; ;; (add-to-list 'delete-frame-functions #'eyebrowse-restore-save-all)
;;     ;; (delq nil (delete-dups delete-frame-functions))
;;     ;; (eyebrowse-restore-mode 1)
;;     ))
