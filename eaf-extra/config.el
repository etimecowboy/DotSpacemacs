;;; config.el --- eaf-extra configuration File for Spacemacs
;; Time-stamp: <2022-12-03 Sat 02:17 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:
(with-eval-after-load "eaf"
  (eaf-setq eaf-browser-enable-adblocker "true")
  (setq eaf-proxy-type "http"
        eaf-proxy-host "127.0.0.1"
        eaf-proxy-port "7890"))
