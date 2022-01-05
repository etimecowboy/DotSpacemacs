;;; packages.el --- Chinese-extra Layer packages File for Spacemacs
;; Time-stamp: <2022-01-05 Wed 23:09 by xin on tufg>
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
    ;; (dictionary-tooltip-mode 1)
    ;; (global-dictionary-tooltip-mode 1)
    ;; (spacemacs/declare-prefix "y" "dictionary")
    (spacemacs/set-leader-keys "os" 'dictionary-search)
      ))

(defun chinese-extra/init-typo ()
  (use-package typo
    :defer t
    :init
    (typo-global-mode 1)
    (add-hook 'text-mode-hook 'typo-mode)
    :config
    (spacemacs|diminish typo-mode " ❝" " T")
      ))

;; load typo-suggest
(defun chinese-extra/init-typo-suggest ()
  (use-package typo-suggest
    :config
    (setq typo-suggest-default-search-method 'ispell)
    (setq typo-suggest-suggestion-count 20)
    (setq typo-suggest-timeout 5)
    (spacemacs|diminish typo-suggest-company-mode)
  ))
