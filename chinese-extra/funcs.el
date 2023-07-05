;;; funcs.el --- Chinese-extra Layer functions File for Spacemacs
;; Time-stamp: <2023-07-05 Wed 01:22 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; REF: ~/org/roam/为 emacs 正确配置英文_中文_符号字体的正确方式_emacs_general_emacs_china.org
;; 中英文对齐测试：
;;       1234567890i1l|!0oO{[()]}<>?/¬`'"%^+-~#@*:;\ --> ~~>
;;       一二三四五六七八九十
;;       12345678901234567890
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))

;; (defun xy/set-fonts ()
;;   "Setup different fonts for default, Chinese, and emoji."
;;   ;; (interactive)
;;   (when (display-graphic-p)

;;     ;; Set default font
;;     (cl-loop for font in
;;              '("Cascadia Code"
;;                "Fira Code"
;;                "Iosevka"
;;                "Source Code Pro"
;;                "Consolas"
;;                "Jetbrains Mono"
;;                "Hack"
;;                "Menlo"
;;                "DejaVu Sans Mono")
;;              when (font-installed-p font)
;;              return (set-face-attribute 'default nil :family font :height 130))  ;; 130

;;     ;; Set font for Chinese characters
;;     (cl-loop for font in
;;              '("Adobe Fangsong Std"
;;                "WenQuanYi Micro Hei Mono"
;;                "FZDaHei-B02"
;;                "Microsoft Yahei"
;;                "Adobe Heiti Std"
;;                "FZXiaoBiaoSong-B05"
;;                "Adobe Song Std"
;;                "Adobe Kaiti Std"
;;                "LXGW WenKai Mono GB")
;;              when (font-installed-p font)
;;              return (progn
;;                       (setq face-font-rescale-alist `((,font . 1.2)))  ;; 1.2
;;                       (set-fontset-font t 'han (font-spec :family font))))))

;; 从剪贴板获取内容
(defun clipboard/get ()
  "return the content of clipboard as string"
  (interactive)
  (with-temp-buffer
    (clipboard-yank)
    (buffer-substring-no-properties (point-min) (point-max))))

;; 统一查询接口
(defun xy/online-dict-at-point ()
  "Look up word/region using web service."
  (interactive)
  (cond ((eq chinese-extra-online-dict-backend 'fanyi) (fanyi-dwim2))
        ((eq chinese-extra-online-dict-backend 'youdao-dictionary) (youdao-dictionary-search-at-point+))
        ((eq chinese-extra-online-dict-backend 'dictionary) (dictionary-search))
        ((eq chinese-extra-online-dict-backend 'google-translate) (google-translate-at-point))
        ((eq chinese-extra-online-dict-backend nil) (message "No online dictionary function"))
        (t (message "Install the online EN-CN dictionary package first!"))))

(defun xy/local-dict-at-point ()
  "Look up word/region using local dictionaries."
  (interactive)
  (cond ((eq chinese-extra-local-dict-backend 'sdcv) (sdcv-search-pointer+))
        ((eq chinese-extra-local-dict-backend nil) (message "No local dictionary function"))
        (t (message "Install the local EN-CN dictionary package first!"))))
