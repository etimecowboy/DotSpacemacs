;;; config.el --- dired-extra configuration File for Spacemacs
;; Time-stamp: <2023-03-16 Thu 08:30 by xin on tufg>
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

(add-hook 'dired-mode-hook
          #'(lambda ()
              (dired-hide-details-mode t)
              (when window-system
                (text-scale-decrease 1))))
