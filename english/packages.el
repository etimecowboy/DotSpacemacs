;;; packages.el --- english Layer packages File for Spacemacs
;; Time-stamp: <2022-11-23 Wed 07:36 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GP3
;;
;;; Commentary:
;; This layer add tools for writing in English.
;;
;;; Code:

(setq english-packages
      '(
        dictionary
        ;; typo
        typo-suggest
        ))

(defun english/init-dictionary ()
  (use-package dictionary
    :defer t
    :init
    ;; (dictionary-tooltip-mode 1)
    ;; (global-dictionary-tooltip-mode 1)
    ;; (spacemacs/declare-prefix "y" "dictionary")
    (spacemacs/set-leader-keys "os" 'dictionary-search)
      ))

;; (defun english/init-typo ()
;;   (use-package typo
;;     :defer t
;;     :init
;;     (typo-global-mode 1)
;;     (add-hook 'text-mode-hook 'typo-mode)
;;     :config
;;     (spacemacs|diminish typo-mode " ❝" " T")
;;       ))

;; load typo-suggest
(defun english/init-typo-suggest ()
  (use-package typo-suggest
    :config
    (setq typo-suggest-default-search-method 'datamuse)
    (setq typo-suggest-suggestion-count 20)
    (setq typo-suggest-timeout 5)
    (spacemacs|diminish typo-suggest-company-mode)
  ))
