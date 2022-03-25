;;; packages.el --- emms packages File for Spacemacs
;; Time-stamp: <2022-03-25 Fri 12:04 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq emms-packages
  '(
    emms
    emms-info-mediainfo
    org-emms
    ))

(defun emms/init-emms ()
  (use-package emms)
  :config
  (emms-all)
  (emms-default-players)
  (require 'emms-player-simple)
  (setq emms-source-file-default-directory "~/音乐")
  (spacemacs/set-leader-keys "amb" 'emms-browser)
  (spacemacs/set-leader-keys "amf" 'emms-play-file)
  (spacemacs/set-leader-keys "amd" 'emms-play-directory)
  (spacemacs/set-leader-keys "amp" 'emms-pause)
  (spacemacs/set-leader-keys "amn" 'emms-next)
  (spacemacs/set-leader-keys "amp" 'emms-previous)
  (spacemacs/set-leader-keys "ams" 'emms-stop)
  (spacemacs/set-leader-keys "am=" 'emms-volume-raise)
  (spacemacs/set-leader-keys "am-" 'emms-volume-lower)
  )

(defun emms/init-emms-info-mediainfo ()
  (use-package emms-info-mediainfo)
  )


(defun emms/init-org-emms ()
  (use-package org-emms)
  )
