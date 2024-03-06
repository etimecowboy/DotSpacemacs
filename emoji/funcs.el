;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- emoji Layer funcs.el File for Spacemacs
;; Time-stamp: <2024-03-05 Tue 04:23 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; ;; Emoji fonts setting, Copied from `emoji' layer, `funcs.el'
;; (defun xy/set-emoji-font (frame)
;;     "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;     (or frame (setq frame (selected-frame)))
;;     (when (fboundp 'set-fontset-font)
;;       (cond
;;        ((spacemacs/system-is-mac)
;;         (set-fontset-font t 'symbol
;;                           (font-spec :family "Apple Color Emoji")
;;                           frame 'prepend))
;;        ((spacemacs/system-is-linux)
;;         (set-fontset-font t 'symbol
;;                           ;; original value was "Symbola"
;;                           (font-spec :family "Segoe UI Emoji") ;; working backup: Twitter Color Emoji
;;                           frame 'prepend)))))

(defun xy/set-emoji-font ()
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
       '(#x1f300 . #x1fad0)
     'emoji
     )
   (cond
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Twitter Color Emoji" (font-family-list)) "Twitter Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

;;   (defun xy/set-emoji-font-for-current-frame ()
;;     "Adjust the font settings of current frame so Emacs can display emoji
;; properly."
;;     (interactive)
;;     (xy/set-emoji-font (selected-frame)))
