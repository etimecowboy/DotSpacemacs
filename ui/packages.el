;;; packages.el --- UI layer packages File for Spacemacs
;; Time-stamp: <2023-12-05 Tue 10:17 by xin on tufg>
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
    doom-themes
    emacs-everywhere
    popwin
    holy-mode ;; belongs to spacemacs-boostrap layer
    hybrid-mode ;; belongs to spacemacs-bootstrap layer
    which-key ;; belongs to spacemacs-bootstrap layer
    persistent-scratch ;; belongs to spacemacs-editing layer
    iscroll
    ;; god-mode
    ;; color-theme-sanityinc-tomorrow
    ;; zenburn-theme
    ;; (dockwin :location (recipe :fetcher github :repo "pronobis/dockwin")) ;; too old
    ;; per-buffer-theme ;; overridden by spacemacs-theme
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

(defun ui/init-emacs-everywhere ()
  (use-package emacs-everywhere
    :ensure t
    ))

(defun ui/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :pre-init
    (setq-default popwin:popup-window-width 40
                  popwin:popup-window-height 15)
    :post-config
    (setq popwin:adjust-other-windows t
          popwin:popup-window-position 'left
          popwin:popup-window-width 40
          popwin:popup-window-height 15
          popwin:reuse-window nil)
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

(defun ui/post-init-holy-mode ()
  (spacemacs|diminish holy-mode))

(defun ui/post-init-hybrid-mode ()
  (spacemacs|diminish hybrid-mode))

(defun ui/post-init-which-key ()
  (spacemacs|diminish which-key-mode))

(defun ui/post-init-persistent-scratch ()
  (spacemacs|diminish persistent-scratch-mode))

(defun ui/init-iscroll ()
  (use-package emacs-everywhere
    :defer t
    :hook ((dired-mode
            image-mode
            org-mode
            markdown-mode
            eww-mode
            w3m-mode
            doc-view-mode
            pdf-view-mode
            ) . iscroll-mode)
    :init
    (spacemacs|diminish iscroll-mode)
    ))

;; (defun ui/init-god-mode ()
;;   (use-package god-mode
;;     :defer t
;;     :bind (("<escape>" . god-mode-all)) ;; optional: switch <escape> and <capslock>
;;     :init
;;     (setq god-exempt-major-modes nil
;;           god-exempt-predicates nil
;;           god-mode-enable-function-key-translation nil
;;           god-mode-alist '((nil . "C-")
;;                            ("g" . "M-")
;;                            ("G" . "C-M-")))
;;     ;;(spacemacs|diminish god-mode " ✝" " God") ;; not 
;;     :config
;;     (when (featurep 'which-key)
;;       (which-key-enable-god-mode-support))
;;     ;;(spacemacs|diminish god-mode " ✝" " God")

;;     (custom-set-faces '(god-mode-lighter
;;                         ((t
;;                           (:background "orange red"
;;                                        :foreground "forest green"
;;                                        :weight extra-bold)))))

;;     (defun xy/toggle-cursor-display ()
;;       (if (or god-local-mode buffer-read-only)
;;           (progn
;;             (setq cursor-type 'box)
;;             (setq blink-cursor-interval 0.3)
;;             (blink-cursor-mode 1))
;;         (progn
;;           (setq cursor-type 'bar)
;;           (blink-cursor-mode -1))))

;;     (add-hook 'post-command-hook #'xy/toggle-cursor-display)
;;     ))

;; (defun ui/init-color-theme-sanityinc-tomorrow ()
;;   (use-package color-theme-sanityinc-tomorrow
;;     :defer t
;;     ))

;; (defun ui/init-zenburn-theme ()
;;   (use-package zenburn-theme
;;     :defer t
;;     ))

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
