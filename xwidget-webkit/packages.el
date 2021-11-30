;;; packages.el --- xwidget-webkit packages File for Spacemacs
;; Time-stamp: <2021-12-01 Wed 02:59 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq xwidget-webkit-packages
  '(
    ;; xwidget
    xwwp
    xwwp-follow-link-helm
    ))

;; (defun xwidget-webkit/init-xwidget ()
;;   (use-package xwidget
;;     :custom
;;     (setq xwidget-webkit-bookmark-jump-new-session t)
;;     (setq xwidget-webkit-download-dir "~/下载/")
;;     ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)
;;     ))

(defun xwidget-webkit/init-xwwp ()
  (use-package xwwp)

  (use-package xwwp-follow-link
    :custom
    (xwwp-follow-link-completion-system 'helm)
    :bind (:map xwidget-webkit-mode-map
                ("v" . xwwp-follow-link))))

(defun xwidget-webkit/init-xwwp-follow-link-helm ()
  (use-package xwwp-follow-link-helm)
  )
