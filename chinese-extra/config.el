;;; config.el --- Chinese-extra configuration File for Spacemacs
;; Time-stamp: <2023-03-07 Tue 00:44 by xin on tufg>
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

;; Set default fonts for GUI mode
;; Characters:
;; abcdefghijklmnopqrstuvwxyz
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; oO08 iIlL1 {} [] g9qcGQ ~-+=>
;; Width test:
;; 123456789012345`!@#$%^&*()_+
;; 中文的宽度？。==>
;; (if window-system
;;     ;;(spacemacs//set-monospaced-font "Source Code Pro" "方正楷体_GBK" 10 10)
;;     ;; (xy/set-font-InputMonoCompressed)
;;     ;; (xy/set-font-Consolas)
;;     ;; (xy/set-font-DejaVuSansMono)
;;   )
;; (xy/set-font-SourceCodePro)
;; (xy/set-font-Iosevka)
;; (xy/set-font-FiraCode)
;; (xy/set-font-CascadiaCode)

(add-hook 'window-setup-hook #'xy/set-fonts)
(add-hook 'server-after-make-frame-hook #'xy/set-fonts)
