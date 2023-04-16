;;; funcs.el --- Compleseus-extra Layer functions File for Spacemacs
;; Time-stamp: <2023-04-14 Fri 01:37 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; REF: https://github.com/oantolin/embark/blob/master/README.org
(defun embark-act-noquit ()
  "Run action but don't quit the minibuffer afterwards."
  (interactive)
  (let ((embark-quit-after-action nil))
    (embark-act)))

(defun embark-act-quit ()
  "Run action but don't quit the minibuffer afterwards."
  (interactive)
  (let ((embark-quit-after-action t))
    (embark-act)))

;; 拼音搜索
;; REF:
;; 1. https://emacs-china.org/t/straight-ivy-helm-selectrum/11523/80
;; 2. https://emacs-china.org/t/vertico/17913/2
(defun eh-orderless-regexp (orig_func component)
  (require 'pyim)
  (require 'pyim-cregexp)
  (let ((result (funcall orig_func component)))
    (pyim-cregexp-build result)))

(advice-add 'orderless-regexp :around #'eh-orderless-regexp)

;; REF: https://karthinks.com/software/fifteen-ways-to-use-embark/#open-a-file-as-root-without-losing-your-session
(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

;; Hack to make preview works even vertico-posframe is enabled.
(defun xy/eli-select-image()
  "Select image with a preview posframe."
  (interactive)
  (vertico-posframe-mode -1)
  (eli-select-images)
  )

;; Redefine Spacemacs Yasnippet functions to avoid errors
(defun spacemacs/load-yasnippet ()
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1))

(defun spacemacs/force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))

