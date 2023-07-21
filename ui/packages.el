;;; packages.el --- UI layer packages File for Spacemacs
;; Time-stamp: <2023-07-21 Fri 08:11 by xin on tufg>
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

(defconst ui-packages
  '(
    color-theme-sanityinc-tomorrow
    doom-themes
    zenburn-theme
    emacs-everywhere
    popwin
    ;; (dockwin :location (recipe :fetcher github :repo "pronobis/dockwin"))
    ;; per-buffer-theme ;; overridden by spacemacs-theme
    ))

(defun ui/init-color-theme-sanityinc-tomorrow ()
  (use-package color-theme-sanityinc-tomorrow
    :defer t
    ))

(defun ui/init-doom-themes ()
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

(defun ui/init-zenburn-theme ()
  (use-package zenburn-theme
    :defer t
    ))

(defun ui/init-emacs-everywhere ()
  (use-package emacs-everywhere
    :defer t
    ))

(defun ui/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (setq popwin:adjust-other-windows t
          popwin:popup-window-position 'left
          popwin:popup-window-width 40
          popwin:popup-window-height 15
          popwin:reuse-window nil)
    (setq-default popwin:popup-window-width 40
                  popwin:popup-window-height 15)
    ;; (defun popwin-restore-window-layout ()
    ;;   (winner-redo)
    ;;   (winner-redo))
    ;; (advice-add 'popwin-restore-window-layout
    ;;             :after 'popwin:close-popup-window)
    (define-key popwin:keymap (kbd "L") #'popwin:display-last-buffer)
    (define-key popwin:keymap (kbd "t") #'popwin:popup-buffer-tail)
    (define-key popwin:keymap (kbd "T") #'popwin:find-file-tail)
    (define-key popwin:keymap (kbd "C-g") #'popwin:close-popup-window)
    (define-key popwin:keymap (kbd "k") #'popwin:close-popup-window)
    (define-key popwin:keymap (kbd "q") #'popwin:close-popup-window)

    ;; (popwin-mode 1) ;; already enabled by spacemacs-visual layer
    ))

;; (defun ui/init-dockwin ()
;;   (use-package dockwin
;;     :defer t
;;     ))

;; (defun ui/init-per-buffer-theme ()
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
