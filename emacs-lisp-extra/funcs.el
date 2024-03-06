;;; funcs.el --- emacs-lisp-extra Layer functions File for Spacemacs
;; Time-stamp: <2024-02-24 Sat 10:01 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; REF: https://emacs.stackexchange.com/questions/38008/adding-many-items-to-a-list
(defun add-list-to-list (dst src)
  "Similar to `add-to-list', but accepts a list as 2nd argument"
  (set dst
       (append (eval dst) src)))

;; example:
;; (add-list-to-list 'company-dabbrev-code-modes
;;                   '(c++-mode c-mode php-mode))

;; REF: https://emacs.stackexchange.com/questions/38008/adding-many-items-to-a-list
;; (defun merge-list-to-list (dst src)
;;   "Merges content of the 2nd list with the 1st one"
;;   (set dst
;;        (append (eval dst) src)))
