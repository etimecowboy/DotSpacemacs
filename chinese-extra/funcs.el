;;; funcs.el --- Chinese-extra Layer functions File for Spacemacs
;; Time-stamp: <2024-04-25 Thu 07:45 by xin on tufg>
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
;; (defun font-installed-p (font-name)
;;   "Check if font with FONT-NAME is available."
;;   (if (find-font (font-spec :name font-name))
;;       t
;;     nil))

;; (defun xy/set-fonts ()
;;   "Setup fonts for default, Chinese, and emoji."
;;   (interactive)
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
;;              return (set-face-attribute 'default nil :family font :height 110))  ;; 130

;;     ;; Set font for Chinese characters
;;     (cl-loop for font in
;;              '("FZDaHei-B02"
;;                "Adobe Fangsong Std"
;;                "WenQuanYi Micro Hei Mono"
;;                ;; "FZDaHei-B02"
;;                "Microsoft Yahei"
;;                "Adobe Heiti Std"
;;                "FZXiaoBiaoSong-B05"
;;                "Adobe Song Std"
;;                "Adobe Kaiti Std"
;;                "LXGW WenKai Mono GB")
;;              when (font-installed-p font)
;;              return (progn
;;                       (setq face-font-rescale-alist `((,font . 1.25)))  ;; 1.2
;;                       (set-fontset-font t 'han (font-spec :family font))))))

;; Set chinese font
;;
;; -------------------------------------------------------------------------------------
;;
;; Based on "~/src/spacemacs/layers/+intl/chinese/config.el#set-monospaced-font"
;;
;; ;; Set the monospaced font size when mixed Chinese and English words
;; (defun spacemacs//set-monospaced-font (english chinese english-size chinese-size)
;;   (set-face-attribute 'default nil :font
;;                       (format   "%s:pixelsize=%d"  english english-size))
;;   (dolist (charset '(kana han cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font) charset
;;                       (font-spec :family chinese :size chinese-size))))
;;
;; ;; If the Hiragino Sans GB font is not found in your system, you could call this
;; ;; method in dotspacemacs/user-config function with a different Chinese font name.
;; ;; If you are using macOS, you could put the following code in your dotspacemacs/user-config function.
;; ;; (when (spacemacs/system-is-mac)
;; ;;   (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16))
;;
;; ---------------------------------------------------------------------------------------
;;
;; Modified according to "~/src/Han/config.el#(defun spacemacs\/set-monospaced-font"
;;
;; (defun spacemacs//set-monospaced-font (english chinese english-size chinese-size)
;;   (set-face-attribute 'default nil :font
;;                       (format   "%s:pixelsize=%d"  english english-size))
;;   (set-face-attribute 'fixed-pitch nil :font
;;                       (format   "%s:pixelsize=%d"  english english-size))
;;   (set-face-attribute 'variable-pitch nil :font
;;                       (format   "%s:pixelsize=%d"  "DejaVu Sans Condensed" english-size))
;;   (dolist (charset '(kana han cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font) charset
;;                       (format "-*-%s-*-*-*-*-%d-*-*-*-*-*-*-*" chinese chinese-size))))
;;
;; -----------------------------------------------------------------------------------------
;;
;; Set the monospaced font size when mixed Chinese and English words
;; (defun spacemacs//set-monospaced-font (english chinese english-size chinese-size)
;;   (interactive)
;;   (set-face-attribute 'default nil :font
;;                       (format   "%s:pixelsize=%d"  english english-size))
;;   (set-face-attribute 'fixed-pitch nil :font
;;                       (format   "%s:pixelsize=%d"  english english-size))
;;   (set-face-attribute 'variable-pitch nil :font
;;                       (format   "%s:pixelsize=%d"  "DejaVu Sans Condensed" english-size))
;;   (dolist (charset '(kana han cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font) charset
;;                       (format "-*-%s-*-*-*-*-%d-*-*-*-*-*-*-*" chinese chinese-size)))
;;   (redraw-display))
;;
;; NOTE: Replaced by functions `xy/set-face-font' and `xy/set-cjk-font' of my ui layer.

;; 从剪贴板获取内容
(defun clipboard/get ()
  "return the content of clipboard as string"
  (interactive)
  (with-temp-buffer
    (clipboard-yank)
    (buffer-substring-no-properties (point-min) (point-max))))

;; 统一查询接口
(defun xy/complex-dict-at-point (&optional frame)
  "Look up word/region using a complex dictionary."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (if (display-graphic-p frame)
      (fanyi-dwim2)
    (fanyi-dwim2)))

(defun xy/en-en-dict-at-point (&optional frame)
  "Look up word/region using an English-to-English dictionary."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (dictionary-search (thing-at-point 'word 'no-properties)))


(defun xy/simple-dict-at-point (&optional frame)
  "Look up word/region using a simple dictionary."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (if (display-graphic-p frame)
      (sdcv-search-pointer+) ;; (youdao-dictionary-search-at-point+)))
    ;; (google-translate-at-point)
    (bing-dict-brief (thing-at-point 'word 'no-properties))
    ))


(defun xy/record-word-at-point (&optional frame)
  "Record word/region to a note."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (bing-dict-brief (thing-at-point 'word 'no-properties)))
