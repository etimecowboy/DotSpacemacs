;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- Shell-extra Layer functions File for Spacemacs
;; Time-stamp: <2023-05-15 Mon 16:32 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; Add some more C- M- key sequences that used by terminal apps.
;; zellij lock-mode  prefix
(defun vterm-send-ctrl-g ()
  "Seng `C-g' to the libvterm."
  (interactive)
  (vterm-send-key "g" nil nil t))

;; tmux default prefix
(defun vterm-send-ctrl-b ()
  "Seng `C-b' to the libvterm."
  (interactive)
  (vterm-send-key "b" nil nil t))

;; (defun vterm-send-meta-return ()
;;   "Seng `M-<return>' to the libvterm."
;;   (interactive)
;;   (vterm-send-key "<return>" nil t))

;; (defun term-mode-common-init ()
;;   "The common initialization for term."
;;   (setq-local scroll-margin 0)
;;   (setq-local truncate-lines t)
;;   )

;; 语法高亮显示
;; (defun eshell/bat (file)
;;   "cat FILE with syntax highlight."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (let ((buffer-file-name file))
;;       (delay-mode-hooks
;;         (set-auto-mode)
;;         (font-lock-ensure)))
;;     (buffer-string)))

;; 交互式进入目录
(defun eshell/z ()
  "cd to directory with completion."
  (let ((dir (completing-read "Directory: " (ring-elements eshell-last-dir-ring) nil t)))
    (eshell/cd dir)))

;; ;; 查找文件
;; (defun eshell/f (filename &optional dir)
;;   "Search for files matching FILENAME in either DIR or the
;; current directory."
;;   (let ((cmd (concat
;;               ;; using find
;;               (executable-find "find")
;;               " " (or dir ".")
;;               " -not -path '*/.git*'"            ; ignore .git directory
;;               " -and -not -path 'build'"         ; ignore cmake build directory
;;               " -and -not -path '*/eln-cache*'"  ; ignore eln cache
;;               " -and -type f -and -iname "
;;               "'*" filename "*'")))
;;     (eshell-command-result cmd)))
