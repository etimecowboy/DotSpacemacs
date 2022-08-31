;;; packages.el --- lsp-bridge Layer packages File for Spacemacs
;; Time-stamp: <2022-08-31 Wed 07:32 by xin on tufg>
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

(setq lsp-bridge-packages
      '(
        (lsp-bridge :location (recipe :fetcher github :repo "manateelazycat/lsp-bridge"))
        ))

(defun lsp-bridge/init-lsp-bridge ()
  (use-package lsp-bridge
    :config
    (setq lsp-bridge-c-lsp-server "ccls"
          lsp-bridge-python-lsp-server "pyright"
          lsp-bridge-tex-lsp-server "texlab")
    ))
