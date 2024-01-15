;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- chinese-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2024-01-15 Mon 13:30 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; (spacemacs//set-monospaced-font "Cascadia Code" "LXGW WenKai Mono GB" 11 18)
;; for chinese layer `youdao-dcitionary' package
(spacemacs/declare-prefix "xC" "Chinese")
(spacemacs/declare-prefix "jC" "Chinese")

(spacemacs/set-leader-keys
  "\\" 'toggle-input-method
  "xCc" 'chinese-conv-replace
  "xCC" 'chinese-conv
  "xCp" 'pangu-spacing-mode
  "xCP" 'pangu-spacing-space-current-buffer
  ;; "xCj" 'ace-pinyin-jump-char
  ;; "xCJ" 'ace-pinyin-jump-char-2
  ;; "xCw" 'ace-pinyin-jump-word
  "jCj" 'ace-pinyin-jump-char
  "jCJ" 'ace-pinyin-jump-char-2
  "jCw" 'ace-pinyin-jump-word
  "tP"  'pangu-spacing-mode
  "xCf" 'fanyi-dwim2
  "xCF" 'fanyi-dwim
  "xCh" 'fanyi-from-history
  "xCw" 'fanyi-copy-query-word
  "xgb" 'google-translate-buffer
  "xgs" 'google-translate-smooth-translate
  "xgo" 'google-translate-paragraphs-overlay
  "xgi" 'google-translate-paragraphs-insert
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "P"  'pangu-spacing-org-mode-at-special-region)

;; (global-set-key (kbd "C-x j") 'ace-pinyin-jump-char)
;; (global-set-key (kbd "<f7>") 'toggle-input-method)
(global-set-key (kbd "C-<f8>") 'xy/complex-dict-at-point)
(global-set-key (kbd "M-<f8>") 'xy/en-en-dict-at-point)
(global-set-key (kbd "<f8>") 'xy/simple-dict-at-point)
