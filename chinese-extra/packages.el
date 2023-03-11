;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- Chinese-extra Layer packages File for Spacemacs
;; Time-stamp: <2023-03-10 Fri 08:05 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst chinese-extra-packages
  '(
    rime
    ;; FIXME: cnfonts 使用后出现 emoji 显示错误。
    ;;        Tested with:
    ;; https://unicode.org/Public/emoji/15.0/emoji-test.txt
    ;; cnfonts
    dictionary
    ;; typo
    typo-suggest
    ))

(defun chinese-extra/init-rime ()
  (use-package rime
    :ensure t
    :init
    ;; REF: https://emacs-china.org/t/emacs-rime/18305
    (defun my/rime-predicate-punctuation-next-char-is-paired-p ()
      (if (not (eq (point) (point-max)))
          (and (rime-predicate-current-input-punctuation-p)
               (not (string-match-p
                     (rx (any "\"\(\[\{"))
                     (buffer-substring (point) (1- (point)))))
               (string-match-p
                (rx (any "\}\]\)\""))
                (buffer-substring (point) (1+ (point)))))
        nil))
    :config
    (setq default-input-method "rime"
          rime-show-candidate 'posframe
          rime-cursor "˰"
          rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v" "C-d" "M-d" "C-w" "M-w"
                                       "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>")
          rime-disable-predicates '(
                                    ;; TEST: new group
                                    rime-predicate-after-ascii-char-p
                                    rime-predicate-prog-in-code-p
                                    rime-predicate-in-code-string-p
                                    ;; rime-predicate-evil-mode-p
                                    rime-predicate-ace-window-p
                                    rime-predicate-hydra-p
                                    rime-predicate-punctuation-line-begin-p
                                    rime-predicate-current-uppercase-letter-p
                                    rime-predicate-tex-math-or-command-p
                                    rime-predicate-org-in-src-block-p
                                    my/rime-predicate-punctuation-next-char-is-paired-p
                                    )
          rime-inline-predicates '(
                                   rime-predicate-space-after-cc-p
                                   ;; rime-predicate-space-after-ascii-p
                                   )
          mode-line-mule-info '((:eval (rime-lighter)))
          rime-inline-ascii-trigger 'shift-l
          rime-inline-ascii-holder ?x
          )

    (define-key rime-active-mode-map (kbd "M-j") 'rime-inline-ascii)
    (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)

    (spacemacs/set-leader-keys "\\" 'toggle-input-method)
    ))

(defun chinese-extra/init-dictionary ()
  (use-package dictionary
    :defer t
    ;; :init
    ;; (dictionary-tooltip-mode 1)
    ;; (global-dictionary-tooltip-mode 1)
    ))

;; (defun chinese-extra/init-typo ()
;;   (use-package typo
;;     :defer t
;;     :init
;;     (typo-global-mode 1)
;;     (add-hook 'text-mode-hook 'typo-mode)
;;     :config
;;     (spacemacs|diminish typo-mode " ❝" " T")
;;       ))

;; load typo-suggest
(defun chinese-extra/init-typo-suggest ()
  (use-package typo-suggest
    :config
    (setq typo-suggest-default-search-method 'datamuse)
    (setq typo-suggest-suggestion-count 20)
    (setq typo-suggest-timeout 5)
    (spacemacs|diminish typo-suggest-company-mode)
    ))
