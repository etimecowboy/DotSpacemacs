;;; packages.el --- emms packages File for Spacemacs
;; Time-stamp: <2022-03-23 Wed 01:07 by xin on tufg>
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
  (setq emms-source-file-default-directory "~/mnt/data1/xin/music")
  )

(defun emms/init-emms-info-mediainfo ()
  (use-package emms-info-mediainfo)
  )


(defun emms/init-org-emms ()
  (use-package org-emms)
  )
