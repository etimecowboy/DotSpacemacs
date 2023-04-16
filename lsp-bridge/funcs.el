;;; funcs.el --- lsp-bridge Layer functions File for Spacemacs
;; Time-stamp: <2023-04-10 Mon 14:54 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

(defun xy/toggle-company-lsp-bridge ()
  "Toggle between lsp-bridge(amc) and company as auto-completion frontend."

  (interactive)

  (if (and (bound-and-true-p company-mode)
             (not (bound-and-true-p lsp-bridge-mode)))
      (progn
        (company-mode -1)
        (lsp-bridge-restart-process)
        (lsp-bridge-mode 1))
    (progn
      (lsp-bridge-mode -1)
      (company-mode 1))))


(defun xy/switch-to-work-in-terminal ()
  "Load acm-terminal to use lsp-bridge in a terminal."
  (interactive)
  (unless (display-graphic-p)
    (require 'acm-terminal))
  (xy/toggle-eaf-browser))
