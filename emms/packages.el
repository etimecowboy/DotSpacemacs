;;; packages.el --- emms packages File for Spacemacs
;; Time-stamp: <2022-03-25 Fri 15:09 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq emms-packages
  '(
    emms
    emms-info-mediainfo
    org-emms
    ))

(defun emms/init-emms ()
  (use-package emms)
  :config
  (emms-all)
  (emms-default-players)
  (require 'emms-player-simple)
  (setq emms-source-file-default-directory "~/音乐"
        emms-playlist-mode-center-when-go t
        emms-volume-change-function 'emms-volume-pulse-change)
  (spacemacs/set-leader-keys "amb" 'emms-browser)
  (spacemacs/set-leader-keys "aml" 'emms-playlist-mode-go-popup)
  (spacemacs/set-leader-keys "amf" 'emms-play-file)
  (spacemacs/set-leader-keys "amd" 'emms-play-directory)
  (spacemacs/set-leader-keys "amp" 'emms-pause)
  (spacemacs/set-leader-keys "amn" 'emms-next)
  (spacemacs/set-leader-keys "amp" 'emms-previous)
  (spacemacs/set-leader-keys "ams" 'emms-stop)
  (spacemacs/set-leader-keys "amr" 'emms-shuffle)
  (spacemacs/set-leader-keys "amR" 'emms-random)
  (spacemacs/set-leader-keys "amS" 'emms-show)
  (spacemacs/set-leader-keys "amM" 'emms-display-modes)
  (spacemacs/set-leader-keys "am+" 'emms-volume-raise)
  (spacemacs/set-leader-keys "am-" 'emms-volume-lower)
  (spacemacs/set-leader-keys "am>" 'emms-seek-forward)
  (spacemacs/set-leader-keys "am<" 'emms-seek-backward)
  (spacemacs/set-leader-keys "am=" 'emms-seek-to)
  (spacemacs/set-leader-keys-for-major-mode 'emms-browser-mode
    "g" 'emms-playlist-mode-go)
  )

(defun emms/init-emms-info-mediainfo ()
  (use-package emms-info-mediainfo)
  )

(defun emms/init-org-emms ()
  (use-package org-emms)
  )

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
