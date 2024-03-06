;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- chinese-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2024-03-06 Wed 07:05 by xin on tufg>
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
(spacemacs/declare-prefix "xz" "Chinese")
(spacemacs/declare-prefix "jz" "Chinese")

(spacemacs/set-leader-keys
  "\\" 'toggle-input-method
  "xzc" 'chinese-conv-replace
  "xzC" 'chinese-conv
  "xzp" 'pangu-spacing-mode
  "xzP" 'pangu-spacing-space-current-buffer
  ;; "xzj" 'ace-pinyin-jump-char
  ;; "xzJ" 'ace-pinyin-jump-char-2
  ;; "xzw" 'ace-pinyin-jump-word
  "jzj" 'ace-pinyin-jump-char
  "jzJ" 'ace-pinyin-jump-char-2
  "jzw" 'ace-pinyin-jump-word
  "tP"  'pangu-spacing-mode
  "xzf" 'fanyi-dwim2
  "xzF" 'fanyi-dwim
  "xzh" 'fanyi-from-history
  "xzw" 'fanyi-copy-query-word
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
(global-set-key (kbd "S-<f8>") (lambda () (interactive)
                                 (bing-dict-brief (thing-at-point 'word 'no-properties))))
