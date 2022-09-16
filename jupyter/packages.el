;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- jupyter layer packages file for Spacemacs.
;; Time-stamp: <2022-09-15 Thu 00:26 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq jupyter-packages
      '(
        jupyter
        ))

(defun jupyter/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(jupyter . t))))

(defun jupyter/init-jupyter ()
  (use-package jupyter
    :after ob
    :init
    (with-eval-after-load 'org
      (add-to-list 'org-babel-load-languages '(jupyter . t)))
    :config
    (require 'ob-jupyter)
    ))

;;; packages.el ends here
