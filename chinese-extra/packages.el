;;; packages.el --- Chinese-extra Layer packages File for Spacemacs
;; Time-stamp: <2021-12-27 Mon 18:46 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq chinese-extra-packages
      '(
        dictionary
        typo
        typo-suggest
        ))

(defun chinese-extra/init-dictionary ()
  (use-package dictionary
    :defer t
    :init
    (progn
      ;; (dictionary-tooltip-mode 1)
      ;; (global-dictionary-tooltip-mode 1)
      )))

(defun chinese-extra/init-typo ()
  (use-package typo
    :defer t
    :init
    (typo-global-mode 1)
    (add-hook 'text-mode-hook 'typo-mode)
      ))

;; load typo-suggest
(defun chinese-extra/init-typo-suggest ()
  (use-package typo-suggest
    :config
    (setq typo-suggest-default-search-method 'ispell)
    (setq typo-suggest-suggestion-count 20)
    (setq typo-suggest-timeout 5))
  )
