;;; config.el --- hardhat configuration File for Spacemacs
;; Time-stamp: <2022-11-23 Wed 08:05 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

(with-eval-after-load "hardhat"
  (global-hardhat-mode 1))

(spacemacs|diminish hardhat-mode "  ⓗ " " h")
