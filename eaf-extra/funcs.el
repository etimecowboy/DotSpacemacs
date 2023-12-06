; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- eaf-extra Layer functions File for Spacemacs
;; Time-stamp: <2023-12-06 Wed 14:25 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; override function eaf-ipython-command to use ipython3
;; (defun eaf-ipython-command ()
;;   (if (eaf--called-from-wsl-on-windows-p)
;;       "ipython.exe"
;;     "ipython3"))

;; (defun eaf-terminal (&optional new)
;;   (interactive)
;;   (eaf-terminal-run-command-in-dir
;;    (eaf--generate-terminal-command) (eaf--non-remote-default-directory) new)
;;   )

;; (defun xy/eaf-open-tmux ()
;;   "Open tmux session `default' in current directory.

;; Mainly for running ob-tmux blocks."
;;   (interactive)
;;   (if (executable-find "tmux")
;;       ;; (eaf-terminal-run-command-in-dir
;;       (eaf-pyqterminal-run-command-in-dir
;;        "tmux new-session -A -s default"
;;        (eaf--non-remote-default-directory))
;;     (message "[EAF/terminal] Please install tmux first.")))
