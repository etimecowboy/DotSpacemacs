; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- eaf-extra Layer functions File for Spacemacs
;; Time-stamp: <2023-06-28 Wed 07:25 by xin on tufg>
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
(defun eaf-ipython-command ()
  (if (eaf--called-from-wsl-on-windows-p)
      "ipython.exe"
    "ipython3"))

(defun eaf-terminal (&optional new)
  (interactive)
  (eaf-terminal-run-command-in-dir
   (eaf--generate-terminal-command) (eaf--non-remote-default-directory) new)
  )

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

(defun xy/set-eaf-browser-as-default ()
  "Set the default web browser as eaf-browser"
  (setq ;; browse-url-default-browser 'eaf-open-browser
        browse-url-browser-function 'eaf-open-browser
        engine/browser-function 'eaf-open-browser)
  (message "The default web browser is set to eaf-browser."))

(defun xy/set-google-chrome-as-default ()
  "Set the default web browser to be google-chrome"
  (setq  ;; browse-url-default-browser 'browse-url-generic
         browse-url-browser-function 'browse-url-generic
         engine/browser-function 'browse-url-generic
         browse-url-generic-program "google-chrome")
  (message "The default web browser is set to google-chrome."))


(defun xy/toggle-eaf-browser ()
  "Toggle the default web browser between eaf-browser and google-chrome"
  (interactive)
  (if (eq browse-url-browser-function 'eaf-open-browser)
      (xy/set-google-chrome-as-default)
    (xy/set-eaf-browser-as-default)))

(defun xy/open-current-webpage-in-chrome ()
  "Open current webpage in Chrome browser."
  (interactive)
  (browse-url-chrome eaf--buffer-url))

(defun xy/open-current-webpage-in-eww ()
  "Open current webpage in EWW browser."
  (interactive)
  (eww eaf--buffer-url))
