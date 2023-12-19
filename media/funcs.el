;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- media Layer functions File for Spacemacs
;; Time-stamp: <2023-12-09 Sat 03:58 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; Notification NOT working
;; REF: https://www.emacswiki.org/emacs/EMMS
;; ; choose D-Bus to disseminate messages, if it is running . 
;; (cond
;;  ;; test to see if D-Bus notifications are available
;;  ((if (and (require 'dbus nil t)
;;            (dbus-ping :session "org.freedesktop.Notifications"))
;;       (progn
;;         (setq notify-method 'notify-via-dbus-notifications)
;;         (require 'notifications))))
 ;;  ;; could use the message system otherwise
;;  (t (setq notify-method 'notify-via-message)))

;; (defun notify-via-notifications (title msg icon)
;;   "Send notification with TITLE, MSG via `D-Bus' . "
;;   (notifications-notify
;;    :title title
;;    :body msg
;;    :app-icon icon
;;    :urgency 'low))

;; (defun notify-via-messages (title msg)
;;   "Send notification with TITLE, MSG to message . "
;;   (message "APPOINTMENT: %s" msg))

;; (defun emms-notifications-dbus (track-name)
;;   "Share track name via `D-Bus'                                                . "
;;   (let ((icon "/usr/share/icons/gnome/24x24/categories/applications-multimedia . png"))
;;     (notify-via-notifications "EMMS is now playing:" track-name icon)))

;; (defun emms-notifications-message (track-name)
;;   "Share track name via Emacs minibuffer . "
;;   (message "EMMS is now playing: %s" track-name))
 ;; (setq emms-player-next-function 'emms-notify-and-next)

;; (defun emms-notify-and-next ()
;;   "Send a notification of track and start next . "
;;   (emms-next-noerror)
;;   (let ((track-name (emms-track-description (emms-playlist-current-selected-track))))
;;     (cond
;;      ((eq notify-method 'notify-via-dbus-notifications)
;;       (emms-notifications-dbus track-name))
;;      (t (emms-notifications-message track-name)))))
