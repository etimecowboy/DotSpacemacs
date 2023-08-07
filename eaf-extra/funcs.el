; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- eaf-extra Layer functions File for Spacemacs
;; Time-stamp: <2023-08-07 Mon 01:20 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; override function eaf-ipython-command to use ipython3
;; (defun eaf-ipython-command ()
;;   (if (eaf--called-from-wsl-on-windows-p)
;;       "ipython.exe"
;;     "ipython3"))

;; (defun eaf-terminal (&optional new)
;;   (interactive)
;;   (eaf-terminal-run-command-in-dir
;;    (eaf--generate-terminal-command) (eaf--non-remote-default-directory) new)
;;   )

(defun xy/eaf-open-tmux ()
  "Open tmux session `default' in current directory.

Mainly for running ob-tmux blocks."
  (interactive)
  (if (executable-find "tmux")
      ;; (eaf-terminal-run-command-in-dir
      (eaf-pyqterminal-run-command-in-dir
       "tmux new-session -A -s default"
       (eaf--non-remote-default-directory))
    (message "[EAF/terminal] Please install tmux first.")))

(defun xy/set-eaf-browser-as-default-browser ()
  "Set the default web browser to eaf-browser"
  (interactive)
  (setq browse-url-default-browser 'eaf-open-browser
        browse-url-browser-function 'eaf-open-browser
        browse-url-secondary-browser-function 'browse-url-chrome
        engine/browser-function 'eaf-open-browser
        browse-url-generic 'browse-url-chrome
        browse-url-generic-program "google-chrome" ;; recover default browser
        )
  (message "The default web browser is set to eaf-browser."))

(defun xy/set-google-chrome-as-default-browser ()
  "Set the default web browser to google-chrome"
  (interactive)
  (setq  browse-url-default-browser 'browse-url-generic ;; org-web-tools
         browse-url-browser-function 'browse-url-generic
         browse-url-secondary-browser-function 'browse-url-generic
         engine/browser-function 'browse-url-generic
         browse-url-generic 'browse-url-chrome
         browse-url-generic-program "google-chrome")
  (message "The default web browser is set to google-chrome."))

(defun xy/set-eww-as-default-browser ()
  "Set the default web browser to eww"
  (interactive)
  (setq browse-url-default-browser 'eww-browse-url
        browse-url-browser-function 'eww-browse-url
        browse-url-secondary-browser-function 'browse-url-chrome
        engine/browser-function 'eww-browse-url
        browse-url-generic 'browse-url-text-emacs
        browse-url-generic-program "google-chrome"
        ;; browse-url-text-browser 'lynx ;; default
   )
  (message "The default web browser is set to eww."))

;; (defun xy/toggle-eaf-browser ()
;;   "Toggle the default web browser between eaf-browser and google-chrome"
;;   (interactive)
;;   (if (eq browse-url-browser-function 'eaf-open-browser)
;;       (xy/set-google-chrome-as-default)
;;     (xy/set-eaf-browser-as-default)))

(defun xy/eaf-browser-browse-with-chrome (&optional url)
  "Open current webpage in Chrome browser."
  (interactive)
  (browse-url-chrome (or url eaf--buffer-url)))

(defun xy/eaf-browser-browse-with-eww (&optional url)
  "Open current webpage in EWW browser."
  (interactive)
  (eww-browse (or url eaf--buffer-url)))

(defun xy/eaf-browser-browse-with-elinks (&optional url)
  "Open current webpage in Elinks."
  (interactive)
  (require 'browse-url)
  (let ((browse-url-text-browser "elinks"))
    (browse-url-text-emacs (or url eaf--buffer-url))))

(defun xy/eaf-browser-browse-with-lynx (&optional url)
  "Open current webpage in Lynx."
  (interactive)
  (require 'browse-url)
  (let ((browse-url-text-browser "lynx"))
    (browse-url-text-emacs (or url eaf--buffer-url))))

(defun xy/eww-browse-with-eaf-browser (&optional url)
  "Browse the current URL with eaf-browser."
  (interactive nil eww-mode)
  (require 'eaf-browser)
  (eaf-open-browser (or url (plist-get eww-data :url))))

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
