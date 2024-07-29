;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- workspace Layer functions File for Spacemacs
;; Time-stamp: <2024-06-25 Tue 03:35:31 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

(defun xy/workspace-restore ()
  "Open a saved workspace."
  (interactive)
  (require 'desktop)
  (require 'burly)
  (require 'burly-tabs)
  (xy/desktop-read)
  (when (length> (burly-bookmark-names) 0)
    (call-interactively 'burly-open-bookmark)
    (message "burly-open-bookmark complete."))
  (message "xy/workspace-restore complete."))


(defun xy/workspace-save ()
  "Save current workspace."
  (interactive)
  (require 'desktop)
  (require 'burly)
  (xy/desktop-save)
  (call-interactively 'burly-bookmark-windows)
  (message "xy/workspace-save complete."))


(defun xy/workspace-save-all ()
  "Save current workspace."
  (interactive)
  (require 'desktop)
  (require 'burly)
  (xy/desktop-save)
  (burly-bookmark-frames "Burly: last")
  (message "xy/workspace-save-all complete.\nAll frames are saved to 'Burly: last'."))


(defun xy/desktop-save ()
  "Explicitly save desktop file to `spacemacs-cache-directory'."
  (interactive)
  (require 'desktop)
  ;; (desktop-save-in-desktop-dir)
  (desktop-save spacemacs-cache-directory)
  (message "xy/desktop-save complete."))


(defun xy/desktop-read()
  "Explicitly read desktop file from `spacemacs-cache-directory'."
  (interactive)
  (require 'desktop)
  (desktop-read spacemacs-cache-directory)
  (message "xy/desktop-read complete."))


;; (defun xy/workspace-restore ()
;;   "Restore last workspace."
;;   (interactive)
;;   (if (member "Burly: last" (burly-bookmark-names))
;;       ;; (burly-open-bookmark "Burly: last")
;;       (bookmark-jump "Burly: last")
;;     (message "Last workspace not saved yet.")))

;; (defun xy/workspace-restore ()
;;   "Open the last-opened Burly bookmark.
;; Helpful for, e.g. quickly restoring an overview while working on
;; a project."
;;   (interactive)
;;   (if burly-opened-bookmark-name
;;       (burly-open-bookmark burly-opened-bookmark-name)
;;     (message "No saved workspace yet."))
;;   )

;; ;; (defun xy/set-frame-name (&option frame)
;; ;;   (interactive)
;; ;;   (or frame (setq frame (selected-frame)))
;; ;;   (let (name)
;; ;;     (read-string "Frame name: ")
;; ;;     (set-frame-parameter frame 'name name)
;; ;;   ;; (set-frame-name)
;; ;;   ))

;; (defun xy/set-frame-name ()
;;   (interactive)
;;   (set-frame-name (read-string "Frame name: ")))

;; ;; (defun xy/eyebrowse-save (&option frame)
;; ;;   (interactive)
;; ;;   (or frame (setq frame (selected-frame)))
;; ;;   (let (name)
;; ;;     (setq name (read-string "Workspace name: "))
;; ;;     (set-frame-parameter frame 'name name)
;; ;;     )
;; ;;   (eyebrowse-restore-save))

;; ;; WORKING version
;; ;; FIXME: don't change frame name
;; ;; (defun xy/eyebrowse-save-other-file ()
;; ;;   (interactive)
;; ;;   (let ((name)
;; ;;     (setq name (read-string "Workspace name: "))
;; ;;     (set-frame-name name)
;; ;;     (eyebrowse-restore-save (selected-frame)))))

;; ;; (defun xy/eyebrowse-restore-save-other-file (&optional frame)
;; ;;   "Save an eyebrowse profile."
;; ;;   (interactive)
;; ;;   ;; REF: https://stackoverflow.com/questions/19828843/how-to-retrieve-a-name-of-the-frame-in-emacs
;; ;;   ;; Save Get old frame name
;; ;;   (or frame (setq frame (selected-frame)))
;; ;;   (let ((old-frame-name (substring-no-properties
;; ;;                          (cdr (assoc 'name (frame-parameters)))))
;; ;;         (new-frame-name (read-string "Workspace name: ")))
;; ;;     ;; replace (set-frame-name new-frame-name)
;; ;;     (set-frame-parameter frame 'name new-frame-name)
;; ;;     (eyebrowse-restore-save frame)
;; ;;     (set-frame-parameter frame 'name old-frame-name)))

;; (defun empty-string-p (string)
;;   "Return true if the STRING is empty or nil. Expects string type."
;;   (or (null string)
;;       (zerop (length (string-trim string)))))

;; (defun xy/eyebrowse-restore-save-as (&optional frame-name frame)
;;   "Save eyebrowse profile to a file."
;;   (interactive)
;;   (let* ((frame (or frame (selected-frame)))
;;          ;; Save Get old frame name
;;          ;; REF: https://stackoverflow.com/questions/19828843/how-to-retrieve-a-name-of-the-frame-in-emacs
;;          ;; (old-frame-name (substring-no-properties
;;          ;;                  (cdr (assoc 'name (frame-parameters)))))
;;          (old-frame-name (frame-parameter frame 'name))
;;          (frame-name (or frame-name (read-string "Workspace name: ")))
;;          (frame-name (if (empty-string-p frame-name)
;;                          ;; default name
;;                          (concat
;;                           (format-time-string "%Y%m%d_%H%M%S")
;;                           "-" user-login-name "@" system-name)
;;                        frame-name)))
;;     ;; replace (set-frame-name new-frame-name)
;;     (set-frame-parameter frame 'name frame-name)
;;     (eyebrowse-restore-save frame)
;;     (set-frame-parameter frame 'name old-frame-name)))


;; (defun xy/eyebrowse-restore-save-default (&optional frame)
;;   "Save the default eyebrowse profile."
;;   (interactive)
;;   (let ((frame (or frame (selected-frame)))
;;         (old-frame-name (frame-parameter frame 'name)))
;;     (set-frame-parameter frame 'name
;;                          (concat user-login-name "@" system-name))
;;     (eyebrowse-restore-save frame)
;;     (set-frame-parameter frame 'name old-frame-name))
;;   (message "xy/eyebrowse-restore-save-default complete."))


;; (defun xy/eyebrowse-restore-default ()
;;   "Restore the default eyebrowse profile."
;;   (interactive)
;;   (eyebrowse-restore ;; (concat (format-time-string "%Y-%m-%d_%H%M%S") "@" system-name)
;;    (concat user-login-name "@" system-name))
;;   (message "xy/eyebrowse-restore-default complete."))


;; ;; Restore workspaces
;; (defun xy/workspace-restore ()
;;   "Restore last workspace."
;;   (interactive)
;;   (xy/desktop-read)
;;   (xy/eyebrowse-restore-default)
;;   (message "xy/workspace-restore complete."))


;; ;; Restore workspaces
;; (defun xy/workspace-save ()
;;   "Save current workspace."
;;   (interactive)
;;   (xy/desktop-save)
;;   (xy/eyebrowse-restore-save-default)
;;   (message "xy/workspace-save complete."))
