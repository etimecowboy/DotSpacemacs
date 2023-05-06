;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- Dired-extra Layer functions File for Spacemacs
;; Time-stamp: <2023-05-06 Sat 04:21 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; REF: https://www.emacswiki.org/emacs/DiredGetFileSize
(defun xy/dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(defun xy/pretty-dired-buffer ()
  (interactive)
  (dired-hide-details-mode t)
  (when (display-graphic-p)
    (text-scale-decrease 1)))
