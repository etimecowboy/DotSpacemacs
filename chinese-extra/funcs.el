;;; funcs.el --- Chinese-extra Layer functions File for Spacemacs
;; Time-stamp: <2022-09-24 Sat 07:59 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; REF: ~/org/roam/为emacs正确配置英文_中文_符号字体的正确方式_emacs_general_emacs_china.org
;; 中英文测试：
;;       1234567890i1l|!0oO{[()]}<>?/¬`'"%^+-~#@*:;\ --> ~~>
;;       一二三四五六七八九十一
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))

(defun xy/set-fonts ()
  "Setup different fonts for default, Chinese, symbols, and emoji."
  (interactive)
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code"
                           "Fira Code"
                           "Iosevka"
                           "Source Code Pro"
                           "Consolas"
                           "Jetbrains Mono"
                           "Hack"
                           "Menlo"
                           "DejaVu Sans Mono"
                           )
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height 110))

    ;; FIXME: This cause wrong display of all-the-icons (in treemacs, dired, minibuffer, and etc.)
    ;; Specify font for all unicode characters
    ;; (cl-loop for font in '("Symbols Nerd Font" "Segoe UI Symbol" "Symbola" "Symbol")
    ;;          when (font-installed-p font)
    ;;          return (set-fontset-font t 'unicode font nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Twitter Color Emoji"
                           "NotoEmoji Nerd Font Mono"
                           "Noto Color Emoji"
                           "Apple Color Emoji")
             when (font-installed-p font)
             return (if (>= emacs-major-version 28)
                        (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("FZXiaoBiaoSong-B05" ;; serif font
                           "FZDaHei-B02" ;; sans serif font
                           "Microsoft Yahei"
                           "Adobe Heiti Std"
                           "WenQuanYi Micro Hei Mono"
                           "LXGW WenKai Mono GB"
                           ;; "FZXiaoBiaoSong-B05"
                           "Adobe Fangsong Std"
                           "Adobe Song Std"
                           "Adobe Kaiti Std"
                           )
             when (font-installed-p font)
             return (progn
                      ;; (setq face-font-rescale-alist `((,font . 1.3))) ;; good for wqy-microhei
                      (setq face-font-rescale-alist `((,font . 1.2)))
                      (set-fontset-font t 'han (font-spec :family font))))
    ))
