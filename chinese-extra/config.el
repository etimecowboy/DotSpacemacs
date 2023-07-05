;;; config.el --- Chinese-extra configuration File for Spacemacs
;; Time-stamp: <2023-07-05 Wed 01:20 by xin on tufg>
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

;; (add-hook 'window-setup-hook #'xy/set-fonts)
;; (add-hook 'server-after-make-frame-hook #'xy/set-fonts)

;; TODO: test Chinese mirrors configuration here instead of in user-init()
;; Use elpa mirrors, check README.org in the chinese layer directory.
;; (spacemacs|use-package-add-hook package
;;   :pre-init
;;   (setq configuration-layer-elpa-archives
;;         '(("melpa-cn" . "http://mirrors.bfsu.edu.cn/elpa/melpa/")
;;           ("org-cn" . "http://mirrors.bfsu.edu.cn/elpa/org/")
;;           ("gnu-cn" . "http://mirrors.bfsu.edu.cn/elpa/gnu/")
;;           ("non-gnu" . "https://elpa.nongnu.org/nongnu/"))))
  ;; ;; tuna mirrors
  ;; (setq configuration-layer-elpa-archives
  ;;       `(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ;;         ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
  ;;         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
  ;;         ("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;;         ;; ("sunrise-commander"  .  "https://mirrors.tuna.tsinghua.edu.cn/elpa/sunrise-commander/")
  ;;         ))
  ;; )

;; Variables

(defvar chinese-extra-online-dict-backend 'google-translate
  "Online English-Chinese dictionary backend.
Possible values are `google-translate', `fanyi', `youdao-dictionary',
`dictionary' and `nil'")

(defvar chinese-extra-local-dict-backend nil
  "Local English-Chinese dictionary backend.
Possible values are `nil',and `sdvc'")
