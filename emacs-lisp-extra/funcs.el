;;; funcs.el --- emacs-lisp-extra Layer functions File for Spacemacs
;; Time-stamp: <2024-03-09 Sat 14:02 by xin on tufg>
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

(defun xy/insert-kbd-macro ()
  "Insert keyboard macro to `macro-file'."
  (interactive)
  (if (and (boundp 'macro-file) (file-exists-p macro-file))
      (progn
        (setq macroname (completing-read "Insert kbd macro (name): "
					                               obarray
                                         #'kmacro-keyboard-macro-p
					                               t))
        (with-temp-buffer
          (newline)
          (insert-kbd-macro (intern macroname))
          (write-region (point-min) (point-max) macro-file t)))
    (message "`macro-file' is not exsiting, please define it first.")))


(defun xy/save-last-kmacro (macroname)
  "Save last keyboard macro to `macro-file'."
  (interactive "sName for last kmacro: ")
  (if (and (boundp 'last-kbd-macro) (boundp 'macro-file) (file-exists-p macro-file))
      (let ((sym (intern macroname)))
        (kmacro-name-last-macro sym)
        (kmacro-bind-to-key nil)
        (with-temp-buffer
          (newline)
          (insert-kbd-macro sym)
          ;; (newline)
          ;; (string-match "\\(.*\\)" (where-is 'replace_org_keywords))
          (write-region (point-min) (point-max) macro-file t)))
    (message "`last-kbd-macro' or `macro-file' not exsiting.")))


;; REF: https://emacs.stackexchange.com/questions/54971/associate-and-save-macro-with-current-file-evil-spacemacs
(defun xy/insert-kbd-macro-register (register)
  "insert macro from register. prompt for register key
if no argument passed. you may need to revise inserted s-expression."
  (interactive "cthe register key:")
  (if (and (boundp 'macro-file) (file-exists-p macro-file))
      (let ((macro (cdr (assoc register register-alist))))
        (with-temp-buffer
          (newline)
          (setf last-kbd-macro macro)
          (insert-kbd-macro '##)
          (write-region (point-min) (point-max) macro-file t)))
    (message "`macro-file' not exsiting.")))

;; (defun evil-paste-kbd-macro-advice (&rest argv)
;;   "make evil paste kbd-macro if register content is a macro.
;; this function check whether content macro by:
;;  1. equal to `last-kbd-macro'
;;  2. is a vector but not string
;;  3. contain unprintable character"
;;   (if (and (>= (length argv) 2)
;;            (second argv))
;;       (let* ((register (second argv))
;;              (register-pair (assoc register register-alist))
;;              (content (if register-pair (cdr register-pair))))
;;         (if (and content
;;                  (or (eq last-kbd-macro content)
;;                      (vectorp content)
;;                      (string-match "[^\t[:print:]\n\r]" content)))
;;             (let ((last-kbd-macro content))
;;               (forward-line)
;;               (beginning-of-line)
;;               (insert-kbd-macro '##)
;;               (forward-line -2)
;;               (search-forward "setq last-kbd-macro")
;;               (replace-match "execute-kbd-macro")
;;               t)))))
;; (advice-add 'evil-paste-after :before-until
;;             'evil-paste-kbd-macro-advice)
