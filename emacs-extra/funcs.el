;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- Dired-extra Layer functions File for Spacemacs
;; Time-stamp: <2024-01-07 Sun 14:59 by xin on tufg>
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

;; REF: https://kundeveloper.com/blog/buffer-files/
;;
;; This command will try to rename the current buffer. It'll just open a prompt
;; on the mini-buffer with the current file path that we can edit and press
;; enter to change the name (or move the file).
(defun xy/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; This command will just ask us if we want to remove the current file. After
;; saying yes, the file is gone. Piece of cake.
(defun xy/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
