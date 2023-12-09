;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- chinese-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-12-08 Fri 10:50 by xin on tufg>
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
(spacemacs/declare-prefix "oc" "Chinese")

(spacemacs/set-leader-keys
  "\\" 'toggle-input-method
  "occ" 'chinese-conv-replace
  "ocC" 'chinese-conv
  "tP"  'pangu-spacing-mode
  "ocP" 'pangu-spacing-space-current-buffer
  "ocj" 'ace-pinyin-jump-char
  "ocJ" 'ace-pinyin-jump-char-2
  ;; "ocw" 'ace-pinyin-jump-word
  ;; "jj"  'ace-pinyin-jump-char
  ;; "jj"  'avy-goto-char
  ;; "jJ"  'ace-pinyin-jump-char-2
  "ocf" 'fanyi-dwim2
  "ocF" 'fanyi-dwim
  "och" 'fanyi-from-history
  "ocw" 'fanyi-copy-query-word
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
