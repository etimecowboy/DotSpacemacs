;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- Chinese-extra Layer packages File for Spacemacs
;; Time-stamp: <2023-05-16 Tue 06:50 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst chinese-extra-packages
  '(
    rime
    ;; FIXME: cnfonts 使用后出现 emoji 显示错误。
    ;;        Tested with:
    ;; https://unicode.org/Public/emoji/15.0/emoji-test.txt
    ;; cnfonts
    ;; typo
    ;; typo-suggest ;; requires helm
    pangu-spacing ;; replace config in chinese layer
    (sdcv :location (recipe
                     :fetcher github
                     :repo "manateelazycat/sdcv"
                     ))
    fanyi
    ))

(defun chinese-extra/init-rime ()
  (use-package rime
    :ensure t
    :init
    ;; REF: https://emacs-china.org/t/emacs-rime/18305
    (defun my/rime-predicate-punctuation-next-char-is-paired-p ()
      (if (not (eq (point) (point-max)))
          (and (rime-predicate-current-input-punctuation-p)
               (not (string-match-p
                     (rx (any "\"\(\[\{"))
                     (buffer-substring (point) (1- (point)))))
               (string-match-p
                (rx (any "\}\]\)\""))
                (buffer-substring (point) (1+ (point)))))
        nil))
    :config
    (setq default-input-method "rime"
          rime-show-candidate 'posframe
          rime-cursor "˰"
          rime-translate-keybindings
          '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v"
            "M-v" "C-d" "M-d" "C-w" "M-w"
            "<left>" "<right>" "<up>" "<down>"
            "<prior>" "<next>" "<delete>")
          rime-disable-predicates
          '(;; TEST: new group
            rime-predicate-after-ascii-char-p
            rime-predicate-prog-in-code-p
            rime-predicate-in-code-string-p
            ;; rime-predicate-evil-mode-p
            rime-predicate-ace-window-p
            rime-predicate-hydra-p
            rime-predicate-punctuation-line-begin-p
            rime-predicate-current-uppercase-letter-p
            rime-predicate-tex-math-or-command-p
            rime-predicate-org-in-src-block-p
            my/rime-predicate-punctuation-next-char-is-paired-p)
          rime-inline-predicates
          '(rime-predicate-space-after-cc-p
            ;; rime-predicate-space-after-ascii-p
            )
          mode-line-mule-info
          '((:eval (rime-lighter)))
          rime-inline-ascii-trigger 'shift-l
          rime-inline-ascii-holder ?x
          )

    (define-key rime-active-mode-map (kbd "M-j") 'rime-inline-ascii)
    (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)

    (spacemacs/set-leader-keys "\\" 'toggle-input-method)
    ))

;; (defun chinese-extra/init-typo ()
;;   (use-package typo
;;     :defer t
;;     :init
;;     (typo-global-mode 1)
;;     (add-hook 'text-mode-hook 'typo-mode)
;;     :config
;;     (spacemacs|diminish typo-mode " ❝" " T")
;;       ))

;; load typo-suggest
;; (defun chinese-extra/init-typo-suggest ()
;;   (use-package typo-suggest
;;     :config
;;     (setq typo-suggest-default-search-method 'datamuse)
;;     (setq typo-suggest-suggestion-count 20)
;;     (setq typo-suggest-timeout 5)
;;     (spacemacs|diminish typo-suggest-company-mode)
;;     ))

(defun chinese-extra/init-pangu-spacing ()
  (use-package pangu-spacing
    ;; :defer t
    ;; :init
    ;; (global-pangu-spacing-mode 1)
    ;; (spacemacs|hide-lighter pangu-spacing-mode)
    ;; Always insert `real' space in org-mode.
    ;; (add-hook 'org-mode-hook
    ;;           (lambda ()
    ;;             (setq-local pangu-spacing-real-insert-separtor t)))
    ))


(defun chinese-extra/init-sdcv ()
  (use-package sdcv
    :commands (sdcv-search-pointer+)
    :config
    (setq sdcv-say-word-p t)
    (setq sdcv-dictionary-data-dir (expand-file-name "~/src/stardict/dict"))
    (setq sdcv-dictionary-simple-list
          '("懒虫简明英汉词典"
            "懒虫简明汉英词典"))
    (setq sdcv-dictionary-complete-list
          '("朗道英汉字典5.0"
            "牛津英汉双解美化版"
            "21世纪双语科技词典"
            "quick_eng-zh_CN"
            "新世纪英汉科技大词典"))
    (setq sdcv-tooltip-timeout 10)
    (setq sdcv-fail-notify-string "没找到释义")
    (setq sdcv-tooltip-border-width 2)
    ))

(defun chinese-extra/init-fanyi ()
  (use-package fanyi
    ;; :ensure t
    :commands (fanyi-dwim fanyi-dwim2)
    ;; :bind-keymap ("\e\e =" . fanyi-map)
    ;; :bind (:map fanyi-map
    ;;             ("w" . fanyi-dwim2)
    ;;             ("i" . fanyi-dwim))
    :init
    ;; to support `org-store-link' and `org-insert-link'
    (require 'ol-fanyi)
    ;; 如果当前指针下有单词，选择当前单词，否则选择剪贴板
    (with-eval-after-load 'org-capture
      (add-to-list 'org-capture-templates
                   '("w" "New word" entry (file+olp+datetree "~/org/roam/english_language_inbox.org" "New")
                     "* %^{Input the new word:|%(cond ((with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'word 'no-properties))) ((clipboard/get)))}\n\n[[fanyi:%\\1][%\\1]]\n\n[[http://dict.cn/%\\1][海词：%\\1]]%?"
                     :tree-type day
                     :empty-lines 1
                     :jump-to-captured t)))
    :config
    (defvar fanyi-map nil "keymap for `fanyi")
    (setq fanyi-map (make-sparse-keymap))
    (setq fanyi-sound-player "mpv")
    (add-to-list 'display-buffer-alist
                 '("^\\*fanyi" display-buffer-same-window))
    :custom
    (fanyi-providers '(;; 海词
                       fanyi-haici-provider
                       ;; 有道同义词词典
                       fanyi-youdao-thesaurus-provider
                       ;; ;; Etymonline
                       ;; fanyi-etymon-provider
                       ;; Longman
                       fanyi-longman-provider
                       ;; ;; LibreTranslate
                       ;; fanyi-libre-provider
                       ))
    ))
