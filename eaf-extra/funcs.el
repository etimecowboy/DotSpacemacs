; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- eaf-extra Layer functions File for Spacemacs
;; Time-stamp: <2022-08-22 Mon 03:47 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:


(defun xy/set-eaf-browser-as-default ()
  "Set the default web browser as eaf-browser"
  (interactive)
  (setq ;; browse-url-default-browser 'eaf-open-browser
        browse-url-browser-function 'eaf-open-browser
        engine/browser-function 'eaf-open-browser)
  (message "The default web browser is set to eaf-browser."))


(defun xy/set-google-chrome-as-default ()
  "Set the default web browser to be google-chrome"
  (interactive)
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
