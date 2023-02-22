;;; packages.el --- lsp-bridge Layer packages File for Spacemacs
;; Time-stamp: <2023-02-22 Wed 02:15 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This layer add another lsp client - lsp-bridge
;;
;;; Code:

(defconst lsp-bridge-packages
      '(
        ;; (lsp-bridge :location (recipe :fetcher github :repo "manateelazycat/lsp-bridge"))
        (lsp-bridge :location (recipe
                               :fetcher github
                               :repo "manateelazycat/lsp-bridge"
                               :files ("*" "acm/*" "core/*")
                               ))
        (acm :location (recipe
                        :fetcher github
                        :repo "manateelazycat/lsp-bridge"
                        :files ("acm/*")
                        ))
        ;; FIXME: In GUI mode, acm pop-up menu has nice icons; while in terminal
        ;; mode, it falls back to a pure-text menu. popon and acm-terminal are
        ;; used for this fallback case. Strangely, in
        ;; Emacs29+native-comp+spacemacs, GUI mode menu is also affected, and
        ;; leaves me no icons. Current work-around is Commenting out them.
        ;;
        ;; (popon)
        ;; (acm-terminal :location (recipe
        ;;                          :fetcher github
        ;;                          :repo "twlz0ne/acm-terminal"))
        ))

(defun lsp-bridge/init-acm ()
  (use-package acm))

(defun lsp-bridge/init-lsp-bridge ()
  (use-package lsp-bridge
    :hook
    (sh-mode . lsp-bridge-mode)
    (python-mode . lsp-bridge-mode)
    (emacs-lisp-mode . lsp-bridge-mode)
    (lisp-interaction-mode . lsp-bridge-mode)
    (c-mode . lsp-bridge-mode)
    (c++-mode . lsp-bridge-mode)
    (rust-mode . lsp-bridge-mode)
    :config
    (setq lsp-bridge-c-lsp-server "ccls"
          lsp-bridge-python-lsp-server "pyright"
          lsp-bridge-tex-lsp-server "texlab")
    ))

;; (defun lsp-bridge/init-popon ()
;;   (use-package popon))

;; (defun lsp-bridge/init-acm-terminal ()
;;   (use-package acm-terminal))
