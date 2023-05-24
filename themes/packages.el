;;; packages.el --- themes layer packages File for Spacemacs
;; Time-stamp: <2023-05-23 Tue 03:42 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; The official themes-megapack is too big. This layer just add selected themes.
;; Top emacs themes can be found in https://emacsthemes.com/popular/index.html
;;
;;; Code:

(defconst themes-packages
  '(
    color-theme-sanityinc-tomorrow
    doom-themes
    zenburn-theme
    ;; per-buffer-theme ;; overridden by spacemacs-theme
    ))

(defun themes/init-color-theme-sanityinc-tomorrow ()
  (use-package color-theme-sanityinc-tomorrow
    :defer t
    ))

(defun themes/init-doom-themes ()
  (use-package doom-themes
    ;; :ensure t
    :defer t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; (load-theme 'doom-one t)
    ;; Enable flashing mode-line on errors
    ;; (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    ;; (doom-themes-neotree-config)
    ;; or for treemacs users
    ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    ;; (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    ;; (doom-themes-org-config)
    ))

(defun themes/init-zenburn-theme ()
  (use-package zenburn-theme
    :defer t
    ))

;; (defun themes/init-per-buffer-theme ()
;;   (use-package per-buffer-theme
;;     :ensure t
;;     :config
;;     (setq per-buffer-theme/use-timer t)
;;     (setq per-buffer-theme/timer-idle-delay 0.1)
;;     ;; (setq per-buffer-theme/default-theme 'notheme)
;;     (setq per-buffer-theme/themes-alist
;;           '(
;;             ;; ((:theme . dichromacy)
;;             ;;  (:buffernames nil)
;;             ;;  (:modes
;;             ;;   haskell-mode haskell-interactive-mode))
;;             ((:theme . zenburn)
;;              (:buffernames nil)
;;              (:modes vterm-mode ansi-term-mode term-mode eshell-mode))
;;             ))
;;     ))
