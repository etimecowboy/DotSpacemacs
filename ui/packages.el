;;; packages.el --- UI layer packages File for Spacemacs
;; Time-stamp: <2024-01-08 Mon 04:17 by xin on tufg>
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
    github-dark-vscode-theme
    popwin
    holy-mode ;; belongs to spacemacs-boostrap layer
    hybrid-mode ;; belongs to spacemacs-bootstrap layer
    which-key ;; belongs to spacemacs-bootstrap layer
    persistent-scratch ;; belongs to spacemacs-editing layer
    iscroll
    spacious-padding
    breadcrumb
    esup ;; record bootup time
    ;; mini-header-line
    ;; path-headerline-mode
    ;; minibuffer-header
    ;; (elegant :location (recipe :fetcher github :repo "rougier/elegant-emacs"))
    ;; (nano :location (recipe :fetcher github :repo "rougier/nano-emacs"))
    ;; emacs-everywhere ;; FIXME: It does not work in Wayland.
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

(defun ui/init-github-dark-vscode-theme ()
  (use-package github-dark-vscode-theme
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
  (use-package iscroll
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

(defun ui/init-spacious-padding ()
  (use-package spacious-padding
    :defer t
    :custom
    (spacious-padding-widths '(:internal-border-width 10
                               :header-line-width 5
                               :mode-line-width 5
                               :tab-width 4
                               :right-divider-width 10
                               :scroll-bar-width 8))
    ;; :init
    ;; (spacemacs|diminish specious-padding-mode)
    ))


(defun ui/init-breadcrumb ()
  (use-package breadcrumb
    :ensure t
    :custom
    (breadcrumb-imenu-max-length 0.5)
    (breadcrumb-project-max-length 0.5)
    (breadcrumb-imenu-crumb-separator "⮞")
    (breadcrumb-project-crumb-separator "/")
    :config

    ;; NOTE: show header-line instead of mode-line, this makes tmux with status
    ;; bar book better
    
    ;; FIXME: “ (spacemacs/toggle-mode-line-off)”does not work
    (modeline-mode -1)

    ;; NOTE
    (breadcrumb-mode 1)

    (custom-set-faces
     '(breadcrumb-face
       ((t (:extend t :background "dark green" :foreground "white" :height 0.7
                    :slant italic :weight light :underline t :overline t
                    :family "Consolas"
                    ;; :family "TerminusTTF"
                    ;; :family "Courier New"
                    ;; :family "Monospaced"
                    ;; :family "FiraCode Nerd Font Mono"
                    ))))
     '(breadcrumb-project-leaf-face
       ((t (:inherit (breadcrumb-project-crumbs-face
                      mode-line-buffer-id))))))
    ))

;; REF: https://github.com/ksjogo/mini-header-line/blob/master/mini-header-line.el
;; (defun ui/init-mini-header-line ()
;;   (use-package mini-header-line
;;     :ensure t
;;     :config
;;     (mini-header-line-mode 1)))

;; REF: http://emacs.rubikitch.com/path-headerline-mode/
;; (defun ui/init-path-headerline-mode ()
;;   (use-package path-headerline-mode
;;     :defer t
;;     :commands (path-header-line-on
;;                path-header-line-off
;;                path-headerline-mode)
;;     ;; :ensure t
;;     ;; :config
;;     ;; (path-headerline-mode +1)
;;     ))

;; REF: https://github.com/rougier/minibuffer-header
;; (defun ui/init-minibuffer-header ()
;;   (use-package minibuffer-header
;;     :ensure t
;;     :config
;;     (minibuffer-header-mode 1)))

;; (defun ui/init-elegant ()
;;   (use-package elegant))

;; (defun ui/init-nano ()
;;   (use-package nano
;;     :config
;;     (setq nano-font-family-monospaced "Sarasa Mono SC Nerd Font"
;;           nano-font-family-proportional nil
;;           nano-font-size 16)
;;     ))

;; (defun ui/init-emacs-everywhere ()
;;   (use-package emacs-everywhere
;;     :ensure t
;;     ))

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

(defun ui/init-esup ()
  (use-package esup
    :commands esup
    ;; To use MELPA Stable use ":pin melpa-stable",
    ;; :pin melpa
    :config
    (setq esup-user-init-file "~/src/spacemacs/init.el"
          esup-depth 3
          esup-insignificant-time 0.001)
  ))
