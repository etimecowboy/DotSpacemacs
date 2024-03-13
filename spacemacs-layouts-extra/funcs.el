;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- spacemacs-layouts-extra Layer functions File for Spacemacs
;; Time-stamp: <2024-03-12 Tue 01:56 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; (defun xy/set-frame-name (&option frame)
;;   (interactive)
;;   (or frame (setq frame (selected-frame)))
;;   (let (name)
;;     (read-string "Frame name: ")
;;     (set-frame-parameter frame 'name name)
;;   ;; (set-frame-name)
;;   ))

(defun xy/set-frame-name ()
  (interactive)
  (set-frame-name (read-string "Frame name: ")))

;; (defun xy/eyebrowse-save (&option frame)
;;   (interactive)
;;   (or frame (setq frame (selected-frame)))
;;   (let (name)
;;     (setq name (read-string "Workspace name: "))
;;     (set-frame-parameter frame 'name name)
;;     )
;;   (eyebrowse-restore-save))

(defun xy/eyebrowse-save ()
  (interactive)
  (let (name)
    (setq name (read-string "Workspace name: "))
    (set-frame-name name)
    (eyebrowse-restore-save (selected-frame))))

;; Explicitly save to cache directory.
(defun xy/desktop-save ()
  (interactive)
  (desktop-save spacemacs-cache-directory))

;; Explicitly read from cache directory.
(defun xy/desktop-read()
  (interactive)
  (desktop-read spacemacs-cache-directory))
