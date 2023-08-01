;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- Chinese-extra Layer packages File for Spacemacs
;; Time-stamp: <2023-08-01 Tue 07:50 by xin on tufg>
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
    pinyinlib
    ace-pinyin
    orderless
    pangu-spacing ;; replace config in chinese layer
    (sdcv :location (recipe :fetcher github
                            :repo "manateelazycat/sdcv"))
    fanyi
    youdao-dictionary
    dictionary
    ;; included in spacemacs-language layer, can be the fallback dictionary
    ;; google-translate
    ;; FIXME: cnfonts 使用后出现 emoji 显示错误。
    ;;        Tested with:
    ;; https://unicode.org/Public/emoji/15.0/emoji-test.txt
    ;; cnfonts
    ;; typo
    ;; typo-suggest ;; requires helm
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

    ;; (spacemacs/set-leader-keys "\\" 'toggle-input-method)
    ))

(defun chinese-extra/init-pinyinlib ()
  (use-package pinyinlib))

(defun chinese-extra/pre-init-orderless ()
  (spacemacs|use-package-add-hook orderless
    :post-config
    ;; 拼音搜索
    ;; REF:
    ;;   1. https://emacs-china.org/t/straight-ivy-helm-selectrum/11523/80
    ;;   2. https://emacs-china.org/t/vertico/17913/2
    ;;
    ;; (defun eh-orderless-regexp (orig_func component)
    ;;   (require 'pyim)
    ;;   (require 'pyim-cregexp)
    ;;   (let ((result (funcall orig_func component)))
    ;;     (pyim-cregexp-build result)))
    ;;(advice-add 'orderless-regexp :around #'eh-orderless-regexp)
    ;;
    ;; -------------------------------------------------------------
    ;;
    ;; You can replaced pyim with pinyinlib
    (require 'pinyinlib)
    ;; make completion support pinyin,
    ;;
    ;; REF: https://emacs-china.org/t/vertico/17913/2
    ;;
    ;; -- new completion style
    ;; (defun completion--regex-pinyin (str)
    ;;   (orderless-regexp (pinyinlib-build-regexp-string str)))
    ;; (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
    ;;
    ;; -- add advice
    ;;
    ;; REF: https://emacs-china.org/t/vertico/17913/3
    ;;
    ;; This is preferred because you don't need to change anything else, such as
    ;; rebinding keys and change the API for all usecases. For example, now I
    ;; can use pinyin Initials in filtering the results of: 1.find-file
    ;; 2.org-roam-node-find 3.consult-line/find/... and etc.
    (defun orderless-regexp-pinyin (str)
      (setf (car str) (pinyinlib-build-regexp-string (car str)))
      str)
    (advice-add 'orderless-regexp :filter-args #'orderless-regexp-pinyin)
    ;;
    ;; TODO: integrates pinyinlib with in-buffer searching functions such as isearch
    ;;
    ;; REF: https://emacs-china.org/t/evil-search-pinyin/13455
    ))

(defun chinese-extra/init-pangu-spacing ()
  (use-package pangu-spacing))

(defun chinese-extra/init-sdcv ()
  (use-package sdcv
    :if (eq chinese-extra-local-dict-backend 'sdcv)
    :defer t
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
    (spacemacs/set-leader-keys "ocs" 'sdcv-search-pointer+)

    ;; override `sdcv-say-word' of sdcv.el
    (defun sdcv-say-word (word)
      "Listen to WORD pronunciation."
      (if (featurep 'cocoa)
          (call-process-shell-command
           (format "say %s" word) nil 0)
        (let ((player (or (executable-find "mpg123")
                          (executable-find "mplayer")
                          (executable-find "mpv")
                          )))
          (if player
              (start-process
               player
               nil
               player
               (format "http://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word)))
            (message "mpg123, mplayer or mpv is needed to play word voice")))))
    ))

(defun chinese-extra/init-fanyi ()
  (use-package fanyi
    :if (eq chinese-extra-online-dict-backend 'fanyi)
    :defer t
    :commands (fanyi-dwim fanyi-dwim2)
    ;; :bind-keymap ("\e\e =" . fanyi-map)
    ;; :bind (:map fanyi-map
    ;;             ("w" . fanyi-dwim2)
    ;;             ("i" . fanyi-dwim))
    :init
    (spacemacs|use-package-add-hook org
      :post-config
      ;; To support `org-store-link' and `org-insert-link'
      (require 'ol-fanyi))

    :config
    (defvar fanyi-map nil "keymap for `fanyi")
    (setq fanyi-map (make-sparse-keymap))
    (setq fanyi-sound-player "mpv")
    (add-to-list 'display-buffer-alist
                 '("^\\*fanyi" display-buffer-same-window))
    (spacemacs/set-leader-keys
      "ocf" 'fanyi-dwim2
      "ocF" 'fanyi-dwim
      "och" 'fanyi-from-history
      "ocw" 'fanyi-copy-query-word)

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

(defun chinese-extra/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :if (eq chinese-extra-online-dict-backend 'youdao-dictionary)
    :defer t
    :config
    ;; Enable Cache
    (setq url-automatic-caching t
          ;; Set file path for saving search history
          youdao-dictionary-search-history-file
          (concat spacemacs-cache-directory ".youdao")
          ;; Enable Chinese word segmentation support
          youdao-dictionary-use-chinese-word-segmentation t)
    (spacemacs/set-leader-keys "ocy" 'youdao-dictionary-search-at-point+)
    ))

(defun chinese-extra/init-dictionary ()
  (use-package dictionary
    :if (eq chinese-extra-online-dict-backend 'dictionary)
    :defer t
    ;; :init
    ;; (dictionary-tooltip-mode 1)
    ;; (global-dictionary-tooltip-mode 1)
    :config
    (spacemacs/set-leader-keys "ocd" 'dictionary-search)
    ))

;; (defun chinese-extra/pre-init-google-translate ()
;;   (spacemacs|use-package-add-hook google-translate
;;     ;; :post-init
;;     ;; (add-hook 'after-save-hook #'google-translate-paragraphs-overlay)
;;     ;; :post-config
;;     ;; (spacemacs/set-leader-keys
;;     ;;   "xgb" 'google-translate-buffer
;;     ;;   "xgs" 'google-translate-smooth-translate
;;     ;;   "xgo" 'google-translate-paragraphs-overlay
;;     ;;   "xgi" 'google-translate-paragraphs-insert)
;;   ))

(defun chinese-extra/init-ace-pinyin ()
  (use-package ace-pinyin
    :ensure t
    :config
    (setq ace-pinyin-simplified-chinese-only-p nil ;; Traditional Chinese Characters Support
          ace-pinyin-treat-word-as-char nil ;; disable Word Jumping Support
          ace-pinyin-enable-punctuation-translation t ;; Enable Punctuations Translation
          ace-pinyin-use-avy t) ;; nil if you want to use `ace-jump-mode'
    (ace-pinyin-global-mode 1)
    (spacemacs|hide-lighter ace-pinyin-mode)
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
