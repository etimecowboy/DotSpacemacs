;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- jupyter layer packages file for Spacemacs.
;; Time-stamp: <2023-05-10 Wed 14:18 by xin on tufg>
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
    :pre-init
    (add-to-list 'org-babel-load-languages '(jupyter . t))))

(defun jupyter/init-jupyter ()
  (use-package jupyter
    ;; :after ob
    ))

;;; packages.el ends here
