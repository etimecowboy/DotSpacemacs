;;; config.el --- conda-extra configuration File for Spacemacs
;; Time-stamp: <2022-11-23 Wed 09:04 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:
(with-eval-after-load "conda"
  ;; interactive shell support
  (conda-env-initialize-interactive-shells)
  ;; eshell support
  (conda-env-initialize-eshell)
  ;; auto-activation
  (conda-env-autoactivate-mode t)
  ;; automatically activate a conda environment on the opening of a file
  (add-hook 'find-file-hook
            (lambda ()
              (when (bound-and-true-p conda-project-env-path)
                (conda-env-activate-for-buffer)))))
