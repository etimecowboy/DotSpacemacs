;;; packages.el --- browsers layer packages File for Spacemacs
;; Time-stamp: <2023-08-08 Tue 07:41 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;
;;; Code:

(defconst browsers-packages
  '(eaf
    eww
    w3m
    browse-url
    ))

(defun browsers/post-init-eaf ()
  (add-list-to-list 'eaf-browser-keybinding
                    '(("C-c C" . "xy/eaf-browser-browse-with-chrome")
                      ("C-c E" . "xy/eaf-browser-browse-with-eww")
                      ("C-c Y" . "xy/eaf-browser-browse-with-lynx")
                      ("C-c E" . "xy/eaf-browser-browse-with-elinks")
                      ("C-c W" . "xy/eaf-browser-browse-with-w3m")
                      ))
  )

(defun browsers/post-init-eww ()
  (define-key eww-link-keymap (kbd "C-c E") 'xy/eww-browse-with-eaf-browser)
  (define-key eww-mode-map (kbd "C-c E") 'xy/eww-browse-with-eaf-browser)
  (define-key eww-link-keymap (kbd "C-c L") 'xy/eww-browse-with-lynx)
  (define-key eww-mode-map (kbd "C-c L") 'xy/eww-browse-with-lynx)
  (define-key eww-link-keymap (kbd "C-c E") 'xy/eww-browse-with-elinks)
  (define-key eww-mode-map (kbd "C-c E") 'xy/eww-browse-with-elinks)
  (define-key eww-link-keymap (kbd "C-c W") 'xy/eww-browse-with-w3m)
  (define-key eww-mode-map (kbd "C-c W") 'xy/eww-browse-with-w3m)
  )

(defun browsers/init-w3m ()
  (use-package w3m
    :defer t
    :config
    (setq w3m-bookmark-file-coding-system 'utf-8-unix
          ;; w3m-default-coding-system   'utf-8-unix
          ;; w3m-coding-system           'utf-8-unix
          ;; w3m-input-coding-system     'utf-8-unix
          ;; w3m-output-coding-system    'utf-8-unix
          ;; w3m-file-coding-system      'utf-8-unix
          ;; w3m-file-name-coding-system 'utf-8-unix
          ;; w3m-terminal-coding-system  'utf-8-unix
          w3m-default-save-directory "~/下载/w3m/"
          w3m-confirm-leaving-secure-page nil
          w3m-cookie-accept-bad-cookies 'ask
          w3m-default-display-inline-images t
          ;; w3m-command-arguments '("-cookie" "-F"))
          w3m-add-tab-number t
          ;; w3m-favicon-use-cache-file t
          w3m-fill-column 100
          ;; w3m-keep-cache-size 500
          w3m-new-session-in-background t
          ;; w3m-new-session-url "about:blank"
          ;; w3m-prefer-cache t
          ;; w3m-use-cookies t
          ;; w3m-use-ange-ftp t
          ;; w3m-use-favicon nil
          ;; w3m-use-mule-ucs t
          ;; w3m-view-this-url-new-session-in-background t
          )
    ))

(defun browsers/init-browse-url ()
  (use-package browse-url
    :defer t
    ))
