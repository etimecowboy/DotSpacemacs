; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- browsers Layer functions File for Spacemacs
;; Time-stamp: <2023-12-15 Fri 16:27 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; NOTE: flatpaks browsers, e.g., brave, do not work well with org-web-tools

;;; Code:

;; (defun xy/set-eaf-browser-as-default-browser ()
;;   "Set the default web browser to eaf-browser"
;;   (interactive)
;;   (require 'browse-url)
;;   (require 'eaf-browser)
;;   (setq browse-url-browser-function 'eaf-open-browser
;;         browse-url-secondary-browser-function 'browse-url-generic
;;         engine/browser-function 'eaf-open-browser
;;         browse-url-generic 'eaf-open-browser)
;;   (message "The default web browser is set to eaf-browser."))


(defun xy/set-google-chrome-as-default-browser ()
  "Set the default web browser to google-chrome"
  (interactive)
  (require 'browse-url)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-secondary-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic 'browse-url-chrome
        browse-url-generic-program "google-chrome")
  (message "The default web browser is set to google-chrome."))


(defun xy/set-brave-as-default-browser ()
  "Set the default web browser to brave"
  (interactive)
  (require 'browse-url)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-secondary-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic 'browse-url-chrome
        browse-url-generic-program "/var/lib/flatpak/exports/bin/com.brave.Browser"
        ;; browse-url-generic-program "brave"
        )
  (message "The default web browser is set to brave."))


(defun xy/set-eww-as-default-browser ()
  "Set the default web browser to eww"
  (interactive)
  (require 'browse-url)
  (require 'eww)
  (setq browse-url-browser-function 'eww-browse-url
        browse-url-secondary-browser-function 'browse-url-chrome
        engine/browser-function 'eww-browse-url
        browse-url-generic 'browse-url-text-emacs
        ;; browse-url-generic-program "google-chrome"
        ;; browse-url-text-browser 'lynx ;; default
   )
  (message "The default web browser is set to eww."))


(defun xy/set-w3m-as-default-browser ()
  "Set the default web browser to emacs-w3m"
  (interactive)
  (require 'browse-url)
  (require 'w3m)
  (setq browse-url-browser-function 'w3m-browse-url
        browse-url-secondary-browser-function 'browse-url-chrome
        engine/browser-function 'w3m-browse-url
        browse-url-generic 'browse-url-text-emacs
        ;; browse-url-generic-program "google-chrome"
        ;; browse-url-text-browser 'lynx ;; default
        )
  (message "The default web browser is set to w3m."))


;; (defun xy/eaf-browser-browse-with-chrome (&optional url)
;;   "Open current webpage in Chrome browser."
;;   (interactive)
;;   (browse-url-chrome (or url eaf--buffer-url)))


;; (defun xy/eaf-browser-browse-with-brave (&optional url)
;;   "Open current webpage in brave browser."
;;   (interactive)
;;   (let ((browse-url-generic-program "brave"))
;;     (browse-url-generic (or url eaf--buffer-url))))


;; (defun xy/eaf-browser-browse-with-eww (&optional url)
;;   "Open current webpage in EWW browser."
;;   (interactive)
;;   (require 'eww)
;;   (eww-browse-url (or url eaf--buffer-url)))


;; (defun xy/eaf-browser-browse-with-elinks (&optional url)
;;   "Open current webpage in Elinks."
;;   (interactive)
;;   (require 'browse-url)
;;   (let ((browse-url-text-browser "elinks"))
;;     (browse-url-text-emacs (or url eaf--buffer-url))))


;; (defun xy/eaf-browser-browse-with-lynx (&optional url)
;;   "Open current webpage in Lynx."
;;   (interactive)
;;   (require 'browse-url)
;;   (let ((browse-url-text-browser "lynx"))
;;     (browse-url-text-emacs (or url eaf--buffer-url))))


;; (defun xy/eaf-browser-browse-with-w3m (&optional url)
;;   "Open current webpage in emacs-w3m."
;;   (interactive)
;;   (require 'w3m)
;;   (w3m-browse-url (or url eaf--buffer-url)))


;; (defun xy/eww-browse-with-eaf-browser (&optional url)
;;   "Browse the current URL with eaf-browser."
;;   (interactive nil eww-mode)
;;   (require 'eaf-browser)
;;   (eaf-open-browser (or url (plist-get eww-data :url))))


(defun xy/eww-browse-with-chrome (&optional url)
  "Browse the current URL with Chrome."
  (interactive nil eww-mode)
  (browse-url-chrome (or url (plist-get eww-data :url))))


(defun xy/eww-browse-with-brave (&optional url)
  "Browse the current URL with Chrome."
  (interactive nil eww-mode)
  (let ((browse-url-generic-program "brave"))
    (browse-url-generic (or url (plist-get eww-data :url)))))


(defun xy/eww-browse-with-elinks (&optional url)
  "Browse the current URL with Elinks."
  (interactive nil eww-mode)
  (require 'browse-url)
  (let ((browse-url-text-browser "elinks"))
    (browse-url-text-emacs (or url (plist-get eww-data :url)))))


(defun xy/eww-browse-with-lynx (&optional url)
  "Browse the current URL with Lynx."
  (interactive nil eww-mode)
  (require 'browse-url)
  (let ((browse-url-text-browser "lynx"))
    (browse-url-text-emacs (or url (plist-get eww-data :url)))))


(defun xy/eww-browse-with-w3m (&optional url)
  "Browse the current URL with emacs-w3m."
  (interactive nil eww-mode)
  (require 'w3m)
  (w3m-browse-url (or url (plist-get eww-data :url))))


(defun xy/w3m-browse-with-eww (&optional url)
  (interactive nil w3m-mode)
  (require 'eww)
  (eww-browse-url (or url w3m-current-url)))


;; (defun xy/w3m-browse-with-eaf-browser (&optional url)
;;   (interactive nil w3m-mode)
;;   (require 'eaf-browser)
;;   (eaf-open-browser (or url w3m-current-url)))


(defun xy/w3m-browse-with-chrome (&optional url)
  (interactive nil w3m-mode)
  (browse-url-chrome (or url w3m-current-url)))


(defun xy/w3m-browse-with-brave (&optional url)
  (interactive nil w3m-mode)
  (let ((browse-url-generic-program "brave"))
    (browse-url-generic (or url w3m-current-url))))

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
