;;; packages.el --- english Layer packages File for Spacemacs
;; Time-stamp: <2023-03-23 Thu 03:10 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GP3
;;
;;; Commentary:
;; This layer add tools for writing in English.
;;
;;; Code:

(defconst popweb-packages
  '(
    ivy
    (popweb :location
            (recipe
             :fetcher github
             :repo "manateelazycat/popweb"
             :files ("*")
             ))
    dictionary
    ))

;; load ivy
(defun popweb/init-ivy ()
  (use-package ivy))

;; load popweb
(defun popweb/init-popweb ()
  (use-package popweb
    :config
    (setq popweb-ext-dir (concat (file-name-directory (locate-library "popweb")) "extension/"))
    (add-to-list 'load-path (concat popweb-ext-dir "color-picker/"))
    (add-to-list 'load-path (concat popweb-ext-dir "dict/"))
    (add-to-list 'load-path (concat popweb-ext-dir "latex/"))
    (add-to-list 'load-path (concat popweb-ext-dir "org-roam/"))
    (add-to-list 'load-path (concat popweb-ext-dir "url-preview/"))
    ;; (require 'popweb-color-picker)
    (require 'popweb-dict)
    (require 'popweb-latex)
    (require 'popweb-org-roam-link)
    (require 'popweb-url)
    (setq popweb-proxy-type "socks5"
          popweb-proxy-host "127.0.0.1"
          popweb-proxy-port "7890")
    ))

(defun popweb/init-dictionary ()
  (use-package dictionary
    :defer t
    ;; :init
    ;; (dictionary-tooltip-mode 1)
    ;; (global-dictionary-tooltip-mode 1)
    ))
