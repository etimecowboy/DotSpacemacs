;;; funcs.el --- lsp-bridge Layer functions File for Spacemacs
;; Time-stamp: <2023-05-01 Mon 09:07 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; company-mode is removed from my config.
;; (defun xy/toggle-company-lsp-bridge ()
;;   "Toggle between lsp-bridge(amc) and company as auto-completion frontend."

;;   (interactive)

;;   (if (and (bound-and-true-p company-mode)
;;              (not (bound-and-true-p lsp-bridge-mode)))
;;       (progn
;;         (company-mode -1)
;;         (lsp-bridge-restart-process)
;;         (lsp-bridge-mode 1))
;;     (progn
;;       (lsp-bridge-mode -1)
;;       (company-mode 1))))
