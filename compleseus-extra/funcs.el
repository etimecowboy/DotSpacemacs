;;; funcs.el --- Compleseus-extra Layer functions File for Spacemacs
;; Time-stamp: <2023-08-01 Tue 00:50 by xin on tufg>
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

;; Redefine Spacemacs Yasnippet functions to avoid errors
(defun spacemacs/load-yasnippet ()
  (interactive)
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1))

(defun spacemacs/force-yasnippet-off ()
  (interactive)
  (yas-minor-mode -1)
  (setq yas-dont-activate t))

(defun xy/adapt-vertico-posframe-config (&optional frame)
  "Adapt vertico-posframe to work in terminal or graphical envrionment."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (when (featurep 'vertico-posframe)
    (unless (display-graphic-p frame)
      (vertico-posframe-mode -1)))
  )

;; (defun xy/org-open-link-at-point-to-ace-window ()
;;   (interactive)
;;   (require 'ace-window)
;;   (let* ((aw-dispatch-always t)
;;          (embark-quit-after-action t)
;;          (cur (buffer-name))
;;          )
;;     (aw-switch-to-window (aw-select nil))
;;     (switch-to-buffer cur)
;;     (call-interactively 'org-open-at-point)))
