;;; config.el --- Chinese-extra configuration File for Spacemacs
;; Time-stamp: <2021-01-27 Wed 09:51 by xin on legion>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; Variables

;; functions
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
