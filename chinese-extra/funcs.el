;;; funcs.el --- Chinese-extra Layer functions File for Spacemacs
;; Time-stamp: <2022-06-23 Thu 06:56 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;;; Emacs auto font selection for different OS
;; REF: (@url :file-name "http://emacser.com/torture-emacs.htm" :display "emacser")
(defun qiang-font-existsp (font)
  "判断某个字体在系统中是否安装"
  (if (null (x-list-fonts font))
      nil t))

;; Example: 按顺序找到一个字体列表( list ) 中第一个已经安装可用的字体
;; (defvar font-list
;;   '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))
;; ;; (require 'cl) ;; find-if is in common list package
;; (find-if #'qiang-font-existsp font-list)

(defun qiang-make-font-string (font-name font-size)
  "产生带上 font size 信息的 font 描述文本"
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)
  "自动为不同字符集设置字体

english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl)                       ; for find if
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font
         (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                    ;; :size chinese-font-size
                    ;; HACK: don't set fixed size for Chinese fonts, or
                    ;; you won't be able to rescale Chinese characters.
                    )))
    ;; Set the default English font
    ;; NOTE: The following 2 method cannot make the font settig work
    ;; in new frames.
    ;; (set-frame-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute
     'default nil :font en-font)
    ;; Set Chinese font
    ;; NOTE: Do not use 'unicode charset, it will cause the english
    ;; font invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset zh-font))))

;; Example:
;; (qiang-set-font
;;  '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace"
;;    "Courier New" "Courier") ":pixelsize=14"
;;    '("Microsoft Yahei" "文泉驿等宽正黑" "文泉驿等宽微米黑"
;;      "黑体" "新宋体" "宋体") 16)
;; 设置字体 Emacs 会优先选用 Concolas + “雅黑”的组合。
;; 如果“雅黑”没有装的话，就使用“文泉驿等宽正黑”，依此类推。
;; 这份字体配置不用改动就能在不同的操作系统字体环境下面使用。
;; 另注：中文要用大一点的字体，使中文字符的宽度正好等于两倍
;; （整数倍）英文字符，才能配合 org mode 下的 table，不至于
;; 对不齐。 测试：
;;       1234567890
;;       一二三四五
;;       i1l|!0oO{[()]}<>?/¬`'"%^+-~#@*:;\ --> ~~>

;; mplus + FZYiHei, loose alignment after scaling
(defun xy/set-font-mplus()
  "Set font. Small and narrow font that is perfect for coding on a vertical monitor."
  (interactive)
  (setq xy:english-fonts '("mplus Nerd Font"))
  (setq xy:chinese-fonts '("FZYiHei-M20S"))

  (when window-system
    (setq scalable-fonts-allowed t    ;; Use scalable fonts
          text-scale-mode-step   1.1) ;; default 1.2
    (setq face-font-rescale-alist
          '(("mplus Nerd Font" . 1.1)
            ("FZYiHei-M20S" . 1.2)
            ))
    (qiang-set-font xy:english-fonts 10 xy:chinese-fonts)))

;; InputMonoCompressed + FZMeiHei, loose alignment after scaling
(defun xy/set-font-InputMonoCompressed()
  "Set font. Compressed terminal font that is perfect for shell operations."
  (interactive)
  (setq xy:english-fonts '("Input Mono Compressed"))
  (setq xy:chinese-fonts '("FZMeiHei-M07"))

  (when window-system
    (setq scalable-fonts-allowed t    ;; Use scalable fonts
          text-scale-mode-step   1.1) ;; default 1.2
    (setq face-font-rescale-alist
          '(("Input Mono Compressed" . 1.1)
            ("FZMeiHei-M07" . 1.25) ;; scaling does not work on this font
            ))
    (qiang-set-font xy:english-fonts 10 xy:chinese-fonts)))

;; DejaVu Sans Mono + Microsoft YaHei
(defun xy/set-font-DejaVuSansMono()
  "Set font. Open source font that was ranked as the best font for coding."
  (interactive)
  (setq xy:english-fonts
        '("DejaVuSansMonoForPowerline Nerd Font"
          "DejaVu Sans Mono for Powerline"
          "DejaVu Sans Mono" ;; in case there is no nerd nor powerline fonts
         ))
  (setq xy:chinese-fonts '("Microsoft YaHei" "微软雅黑"))
  ;; (setq xy:chinese-fonts '("FZMeiHei-M07"))

  (when window-system
    (setq scalable-fonts-allowed t    ;; Use scalable fonts
          text-scale-mode-step   1.1) ;; default 1.2
    (setq face-font-rescale-alist
          '(("DejaVuSansMonoForPowerline Nerd Font" . 1.0)
            ("DejaVu Sans Mono for Powerline" . 1.0)
            ("DejaVu Sans Mono" . 1.0)
            ("Microsoft YaHei" . 1.25)
            ("微软雅黑" . 1.25)
            ))
    (qiang-set-font xy:english-fonts 10 xy:chinese-fonts)))

;; Consolas + Microsoft YaHei
(defun xy/set-font-Consolas()
  "Set microsoft coding font that is aviable on Windows Vista or above versions."
  (interactive)
  (setq xy:english-fonts
        '("Consolas"))
  (setq xy:chinese-fonts '("Microsoft YaHei" "微软雅黑"))

  (when window-system
    (setq scalable-fonts-allowed t    ;; Use scalable fonts
          text-scale-mode-step   1.1) ;; default 1.2
    (setq face-font-rescale-alist
          '(("Consolas" . 1.0)
            ("Microsoft YaHei" . 1.1)
            ("微软雅黑" . 1.1)
            ))
    (qiang-set-font xy:english-fonts 10 xy:chinese-fonts)))

;; Source Code Pro + 方正粗圆
(defun xy/set-font-SourceCodePro()
  "Set Source Code Pro and FZCuYuan."
  (interactive)
  (setq xy:english-fonts '("SauceCodePro Nerd Font"))
  ;; (setq xy:chinese-fonts '("Microsoft YaHei" "微软雅黑"))
  (setq xy:chinese-fonts '("方正粗圆_GBK"))

  (when window-system
    (setq scalable-fonts-allowed t    ;; Use scalable fonts
          text-scale-mode-step   1.1) ;; default 1.2
    (setq face-font-rescale-alist
          '(("SauceCodePro Nerd Font" . 1.0)
            ("方正粗圆_GBK" . 1.25)
            ))
    (qiang-set-font xy:english-fonts 10 xy:chinese-fonts)))

;; Iosevka Nerd Font Mono + 方正粗圆
(defun xy/set-font-Iosevka()
  "Set Iosevka and FZCuYuan."
  (interactive)
  (setq xy:english-fonts '("Iosevka Nerd Font Mono"))
  (setq xy:chinese-fonts '("方正粗圆_GBK"))

  (when window-system
    (setq scalable-fonts-allowed t    ;; Use scalable fonts
          text-scale-mode-step   1.1) ;; default 1.2
    (setq face-font-rescale-alist
          '(("Iosevka Nerd Font Mono" . 1.0)
            ("方正粗圆_GBK" . 1.0) ;; good for font size 12
            ))
    (qiang-set-font xy:english-fonts 12 xy:chinese-fonts)))

;; FiraCode Nerd Font Mono + Adobe 黑体
(defun xy/set-font-FiraCode()
  "Set FiraCode and Adobe HeiTi."
  (interactive)
  (setq xy:english-fonts '("FiraCode Nerd Font Mono"))
  (setq xy:chinese-fonts '("Adobe 黑体 Std"))

  (when window-system
    (setq scalable-fonts-allowed t    ;; Use scalable fonts
          text-scale-mode-step   1.2) ;; default 1.2
    (setq face-font-rescale-alist
          '(("FiraCode Nerd Font Mono" . 1.0)
            ("Adobe 黑体 Std" . 1.25)))
    (qiang-set-font xy:english-fonts 11 xy:chinese-fonts)))

;; CascadiaCode + Microsoft YaHei
(defun xy/set-font-CascadiaCode()
  "Set CascadiaCode and Microsoft YaHei."
  (interactive)
  (setq xy:english-fonts '("Cascadia Code"))
  (setq xy:chinese-fonts '("Microsoft YaHei"))

  (when window-system
    (setq scalable-fonts-allowed t    ;; Use scalable fonts
          text-scale-mode-step   1.1) ;; default 1.2
    (setq face-font-rescale-alist
          '(("Cascadia Code" . 1.0)
            ("Microsoft YaHei" . 1.25)))
    (qiang-set-font xy:english-fonts 10 xy:chinese-fonts)))
