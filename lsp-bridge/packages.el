;;; packages.el --- lsp-bridge Layer packages File for Spacemacs
;; Time-stamp: <2023-03-25 Sat 13:01 by xin on tufg>
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
        (lsp-bridge :location (recipe
                               :fetcher github
                               :repo "manateelazycat/lsp-bridge"
                               :files ("*") ;;  "acm/*" "core/*")
                               ))
        (popon :location (recipe
                          :fetcher git
                          :url "https://codeberg.org/akib/emacs-popon.git"))
        (acm-terminal :location (recipe
                                 :fetcher github
                                 :repo "twlz0ne/acm-terminal"))
        ))

(defun lsp-bridge/init-lsp-bridge ()
  (use-package lsp-bridge
    :commands
    (lsp-bridge-mode lsp-bridge-restart-process)
    :hook
    (sh-mode . lsp-bridge-mode)
    (python-mode . lsp-bridge-mode)
    (emacs-lisp-mode . lsp-bridge-mode)
    (lisp-interaction-mode . lsp-bridge-mode)
    (c-mode . lsp-bridge-mode)
    (c++-mode . lsp-bridge-mode)
    (rust-mode . lsp-bridge-mode)
    ;; (org-mode . lsp-bridge-mode)
    ;; :bind (:map acm-mode-map
    ;;             ("SPC" . acm-complete))
    ;; NOTE: <tab> is better than <SPC> as the completion key
    :config
    (setq lsp-bridge-dir (file-name-directory (locate-library "lsp-bridge")))
    (add-to-list 'load-path (concat lsp-bridge-dir "core/"))
    (add-to-list 'load-path (concat lsp-bridge-dir "acm/"))
    (setq lsp-bridge-c-lsp-server "ccls"
          lsp-bridge-python-lsp-server "pyright_ruff"
          lsp-bridge-tex-lsp-server "texlab"
          lsp-bridge-use-ds-pinyin-in-org-mode t)
    (setq acm-enable-quick-access t
          acm-quick-access-modifier 'control)
    ))

(defun lsp-bridge/init-popon ()
  (use-package popon))

(defun lsp-bridge/init-acm-terminal ()
  (use-package acm-terminal
    :after lsp-bridge
    :config
    (require 'acm-terminal)
    ))
