;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- ui Layer functions File for Spacemacs
;; Time-stamp: <2024-07-19 Fri 03:06:03 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))

;; Set basic font that other faces are inherit from
(defun xy/set-face-font (face-symbol font-name &optional font-size)
  "Set the font for `face-symbol' face."
  (or font-size (setq font-size nil))
  (when (font-installed-p font-name)
    (if font-size
        (set-face-attribute face-symbol nil :family font-name :height font-size)
      (set-face-attribute face-symbol nil :family font-name))))

(defun xy/set-cjk-font (font-name &optional font-size)
  "Set the fonts for CJK character sets."
  (or font-size (setq font-size nil))
  (when (font-installed-p font-name)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family font-name)))) ;; :size font-size
  (when font-size
    (setq face-font-rescale-alist '((font-name . font-size)))
    ;;(delq nil (delete-dups face-font-rescale-alist))
    ))

(defun xy/reset-fonts ()
  "Reset fonts of some faces to default."
  (xy/set-face-font 'default xy:default-font xy:default-size)
  (xy/set-face-font 'fixed-pitch xy:fixed-pitch-font xy:fixed-pitch-size)
  (xy/set-face-font 'fixed-pitch-serif xy:fixed-pitch-serif-font xy:fixed-pitch-serif-size)
  (xy/set-face-font 'variable-pitch xy:variable-pitch-font xy:variable-pitch-size)
  (xy/set-face-font 'variable-pitch-text xy:variable-pitch-font xy:variable-pitch-size)
  (xy/set-cjk-font xy:cjk-font xy:cjk-size)
  (message "Emacs font reset.")
  (redraw-display))

;; (defun xy/set-basic-faces (default-font fixed-font fixed-serif-font variable-font variable-text-height)
;;   "Configure fixed- and variable-pitch faces."
;;   (set-face-attribute 'default nil :family default-font :height 120 :weight 'normal)
;;   (set-face-attribute 'fixed-pitch nil :family fixed-font :height 120)
;;   (set-face-attribute 'fixed-pitch-serif nil :family fixed-serif-font :height 120)
;;   (set-face-attribute 'variable-pitch nil :family variable-font :height 160)
;;   (set-face-attribute 'variable-pitch-text nil :height variable-text-height))

;; Show full path file name
;; REF: https://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(defun xy/show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

;;;###autoload
(define-minor-mode modeline-mode
  "Displaying modeline."
  :init-value t
  :global t
  :group 'modeline-mode
  ;; hide mode line
  ;; from http://bzg.fr/emacs-hide-mode-line.html
  ;; loaded in spacemacs-core
  (require 'core-funcs) ;;
  (if (not modeline-mode)
      (progn
        (add-hook 'buffer-list-update-hook #'hidden-mode-line-mode)
        (hidden-mode-line-mode -1))
    (progn
      (remove-hook 'buffer-list-update-hook #'hidden-mode-line-mode)
      (hidden-mode-line-mode 1)
      ))
  ;; (force-mode-line-update)
  ;; (redraw-display)
  ;; (revert-buffer)
  )

;;;###autoload
(define-minor-mode global-tabs-mode
  "Displaying tabs in all frames."
  :init-value nil
  :global t
  :group 'global-tabs-mode
  (require 'tab-bar)
  (require 'tab-line)
  (if global-tabs-mode
      (progn
        (tab-bar-mode 1)
        ;; (xy/tabbar-setup) ;; hooked to `tab-bar-mode'
        (tab-line-mode 1)
        (global-tab-line-mode 1)
        ;; (xy/tabline-setup) ;; hooked to `tab-line-mode' and `global-tab-line-mode'
        )
    (progn
      (tab-bar-mode -1)
      (tab-line-mode -1)
      (global-tab-line-mode -1)))
  ;; Force a mode-line update because it updates the tab line as well.
  (force-mode-line-update t))

;; NOTE: default size is copied from .spacemacs
;;
;; TODO: use height and weight in `default-frame-alist'
;;
;;     Test: (assq 'height default-frame-alist)
;;
;; TODO: restore the frame size before calling this command
(defun xy/restore-frame-size (&optional frame)
  "Restore default frame size."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (when (display-graphic-p frame)
    (set-frame-size frame 115 25)))

;; (defun xy/toggle-breadcrumb ()
;;   "Toggle breadcrumb header line in current buffer."
;;   (interactive)
;;   (require 'breadcrumb)
;;   (if breadcrumb-local-mode
;;       (progn
;;         (when (featurep 'tab-line) (tab-line-mode 1))
;;         (breadcrumb-local-mode -1))
;;     (progn
;;       (when (featurep 'tab-line) (tab-line-mode -1))
;;       (breadcrumb-local-mode 1)))
;;   (force-mode-line-update t))

;; (defun xy/toggle-global-breadcrumb ()
;;   "Toggle breadcrumb header line globally."
;;   (interactive)
;;   (require 'breadcrumb)
;;   (if (or breadcrumb-local-mode breadcrumb-mode)
;;       (progn
;;         (when (featurep 'tab-line) (global-tab-line-mode 1))
;;         (breadcrumb-mode -1))
;;     (progn
;;       (when (featurep 'tab-line) (global-tab-line-mode -1))
;;       (breadcrumb-mode 1)))
;;   (force-mode-line-update t))

(defun xy/close-treemacs-window ()
  "Close treemacs window if it was displayed in current frame."
  (interactive)
  (require 'treemacs)
  (treemacs-select-window)
  (treemacs-quit))

(defun xy/open-treemacs-window ()
  "Open treemacs window in current workspace."
  (interactive)
  (require 'treemacs)
  (treemacs-select-window)
  (treemacs-select-window))

;; (defun xy/toggle-tabs ()
;;   "Toggle all tabs in current window."
;;   (interactive)
;;   (require 'tab-bar)
;;   (require 'tab-line)
;;   (xy/tabbar-setup)
;;   (toggle-frame-tab-bar)
;;   (if tab-line-mode
;;       (tab-line-mode -1)
;;     (progn
;;       (tab-line-mode 1)
;;       (xy/tabline-setup)))
;;   (when (featurep 'breadcrumb)
;;     (breadcrumb-local-mode -1))
;;   ;; Force a mode-line update because it updates the tab line as well.
;;   (force-mode-line-update t))

;; (defvar tab-status 1)

;; (defun xy/global-tabs (&optional state)
;;   "Toggle all tabs globally."
;;   (interactive)
;;   (require 'tab-bar)
;;   (require 'tab-line)
;;   (require 'breadcrumb)
;;   (let ((state (or state tab-status)))
;;     (if (or (= state -1) tab-bar-mode global-tab-line-mode tab-line-mode breadcrumb-mode)
;;         (progn
;;           (tab-bar-mode -1)
;;           (tab-line-mode -1)
;;           (global-tab-line-mode -1)
;;           (breadcrumb-mode -1)
;;           (setq tab-state -1))
;;       (progn
;;         (tab-bar-mode 1)
;;         (tab-line-mode 1)
;;         (global-tab-line-mode 1)
;;         (breadcrumb-mode -1)
;;         (xy/tabbar-setup)
;;         (xy/tabline-setup)
;;         (setq tab-state 1)))
;;     ;; Force a mode-line update because it updates the tab line as well.
;;     (force-mode-line-update t)))

;; (defun xy/turn-on-global-tabs ()
;;   "Turn on all tabs globally."
;;   (interactive)
;;   (require 'tab-bar)
;;   (require 'tab-line)
;;   (modeline-mode -1)
;;   (spacemacs/toggle-mode-line-off)
;;   (tab-bar-mode 1)
;;   (xy/tabbar-setup)
;;   (global-tab-line-mode 1)
;;   (xy/tabline-setup)
;;   (when (featurep 'breadcrumb) (breadcrumb-mode -1))
;;   (force-mode-line-update t))

;; (defun xy/turn-off-global-tabs ()
;;   "Turn off all tabs globally."
;;   (interactive)
;;   (require 'tab-bar)
;;   (require 'tab-line)
;;   (tab-bar-mode -1)
;;   (global-tab-line-mode -1)
;;   (force-mode-line-update t))

(defun xy/ide-gui ()
  "Turn on IDE GUI."
  (interactive)
  (modeline-mode -1)
  (winum-mode -1)
  (spacemacs/toggle-mode-line-off)
  (breadcrumb-mode 1)
  (global-tabs-mode 1)
  (xy/open-treemacs-window)
  (force-mode-line-update t))

(defun xy/tabs-gui ()
  "Turn on tabs GUI."
  (interactive)
  (modeline-mode -1)
  (winum-mode -1)
  (breadcrumb-mode -1)
  (spacemacs/toggle-mode-line-off)
  (global-tabs-mode 1)
  (xy/close-treemacs-window)
  (force-mode-line-update t))

(defun xy/default-gui ()
  "Turn on default GUI."
  (interactive)
  (modeline-mode 1)
  (winum-mode 1)
  (spacemacs/toggle-mode-line-on)
  (breadcrumb-mode -1)
  (global-tabs-mode -1)
  (xy/close-treemacs-window)
  (force-mode-line-update t)
  (save-buffer)
  (revert-buffer t t))

(defun xy/mini-gui ()
  "Turn on mini GUI."
  (interactive)
  (modeline-mode -1)
  (winum-mode -1)
  (spacemacs/toggle-mode-line-off)
  (breadcrumb-mode 1)
  (global-tabs-mode -1)
  (xy/close-treemacs-window)
  (force-mode-line-update t))


(defun xy/adapt-which-key-posframe-config (&optional frame)
  "Adapt which-key-posframe to work in terminal or graphical envrionment."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (require 'which-key)
  (require 'which-key-posframe)
  (if (display-graphic-p frame)
      (which-key-posframe-mode 1)
    (which-key-posframe-mode -1)))


(defun xy/adapt-ui-config (&optional frame)
  "Adapt UI to work in terminal or graphical environment."
  (interactive)
  (require 'color)
  ;; NOTE: Set time locale to UNIX standard, so that timestamps do not use
  ;; chinese day-of-week. It also can be solved by
  ;;
  ;;     (setenv "LC_TIME" "C")
  ;;
  (setq-default system-time-locale "C")
  (setq system-time-locale "C")
  ;; FIXME: Although `system-time-locale' has been set in
  ;; `dotspacemacs/user-env', it does not kick in emacsclient frames when emacs
  ;; was launched as a server.
  ;;
  ;; Reset in `xy/adapt-ui-config' function, so that it will be run whenever a
  ;; new frame was created.
  ;;
  ;; However, this is still not set. I put it in `xy/adapt-org-config' as well

  (or frame (setq frame (selected-frame)))
  (if (display-graphic-p frame)
      (progn
        ;; color-theme
        ;; (set-face-background 'default "unspecified-bg")
        ;; (set-face-background 'font-lock-comment-face "unspecified-bg")

        ;; color names are still required by many other spacemacs components,
        ;; when you don't use spacemacs-theme
        ;; (require 'spacemacs-common)

        (when (featurep 'doom-manegarm-theme)
          ;; Change mode-line color, so that vertically windows are
          ;; well-separated
          (set-face-foreground 'mode-line "chocolate")
          (set-face-background 'mode-line "lime green")
          (set-face-foreground 'mode-line-inactive "khaki")
          (set-face-background 'mode-line-inactive "sea green")
          (custom-set-faces
           ;; '(hl-line ((t (:background "gray20" ;;"OrangeRed4"
           ;;                            :extend t))))
           '(vertico-current ((t (:extend t :background "OrangeRed4"))))))

        (when (featurep 'modus-themes)
          ;; Change mode-line color, so that vertically windows are
          ;; well-separated
          (set-face-foreground 'mode-line "white")
          (set-face-background 'mode-line "dim gray")
          (set-face-foreground 'mode-line-inactive "gray30")
          (set-face-background 'mode-line-inactive "SlateGray4"))

        ;; (when (featurep 'spacemacs-theme)
        ;;   (set-face-background 'default "unspecified-bg")
        ;;   (set-face-background 'font-lock-comment-face "unspecified-bg")
        ;;   (custom-set-faces
        ;;    '(tab-line ((t (:background "dark violet"
        ;;                                :foreground "white"
        ;;                                :family "Sarasa Mono SC Nerd Font"
        ;;                                ))))
        ;;    '(tab-line-highlight ((t (:inherit tab-line
        ;;                                       :background "magenta"
        ;;                                       :foreground "#b2b2b2"
        ;;                                       ))))
        ;;    '(tab-line-tab ((t (:inherit tab-line-highlight
        ;;                                 :box (:style released-button)
        ;;                                 ))))
        ;;    '(tab-line-tab-current ((t (:inherit tab-line-tab
        ;;                                         :background "magenta"
        ;;                                         :weight bold
        ;;                                         :box (:style pressed-button)
        ;;                                         ))))
        ;;    '(tab-line-tab-inactive ((t (:inherit tab-line-tab
        ;;                                          :background "purple"
        ;;                                          :foreground "gray60"
        ;;                                          ))))
        ;;    '(tab-line-tab-modified ((t (:inherit tab-line-tab
        ;;                                          :foreground "yellow"
        ;;                                          ))))
        ;;    '(tab-bar ((t (:background "dark violet"
        ;;                               :foreground "white"
        ;;                               :family "Sarasa Mono SC Nerd Font"
        ;;                               ))))
        ;;    '(tab-bar-tab ((t (:inherit tab-bar
        ;;                                :background "magenta"
        ;;                                :box (:style released-button)))))
        ;;    '(tab-bar-tab-group-current ((t (:inherit tab-bar-tab
        ;;                                              :background "magenta"
        ;;                                              :weight bold
        ;;                                              :box (:style pressed-button)))))
        ;;    '(tab-bar-tab-group-inactive ((t (:inherit tab-bar-tab
        ;;                                               :background "purple"
        ;;                                               :foreground "gray60"))))
        ;;    '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab
        ;;                                         :background "purple"
        ;;                                         :foreground "gray60"))))
        ;;    '(tab-bar-tab-ungrouped ((t (:inherit tab-bar-tab
        ;;                                          :background "purple"
        ;;                                          :foreground "gray60")))))
        ;;   )

        ;; (when (featurep 'spacemacs-theme)
        ;;   (set-face-background 'font-lock-comment-face "unspecified-bg")
        ;;   (custom-set-faces
        ;;    '(tab-line ((t (:background "dark violet"
        ;;                    :foreground "white"
        ;;                    ))))
        ;;    '(tab-line-highlight ((t (:background "grey85" :foreground "black"
        ;;                              :box (:line-width (1 . 1)
        ;;                              :style released-button)))))
        ;;    '(tab-line-tab ((t (:inherit tab-line :box (:line-width (1 . 1)
        ;;                        :style released-button)))))
        ;;    '(tab-line-tab-current ((t (:background "magenta"
        ;;                                :foreground "#b2b2b2"
        ;;                                ))))
        ;;    '(tab-line-tab-inactive ((t (:background "purple"
        ;;                                 :foreground "gray"
        ;;                                 ))))
        ;;    '(tab-line-tab-modified ((t (:foreground "yellow"
        ;;                                ))))
        ;;    ))

        (spacemacs/enable-background-transparency frame)

        ;; Reset fonts
        (xy/reset-fonts)

        ;; `hl-line' package
        ;; (custom-set-faces
        ;;  '(hl-line ((t (:background "color-16" :extend t :underline nil)))))

        ;; -- Try to unify UI-------------------------
        ;; ;; turn on mode-line
        ;; ;; (spacemacs/toggle-mode-line-on)
        ;; ;; (hidden-mode-line-mode -1)
        ;; (modeline-mode 1)
        ;;
        ;; ;; turn off header-line
        ;; ;; (xy/turn-off-header-line)
        ;; ;; (path-headerline-mode-off)
        ;; ;; (path-headerline-mode -1)
        ;; (breadcrumb-mode -1)
        ;;
        ;; ;; Add padding to emacs frame
        ;; ;; (spacious-padding-mode 1)
        ;; --------------------------------------------

        ;; Focus on the new frame
        ;;
        ;; REF: https://askubuntu.com/questions/283711/application-focus-of-emacsclient-frame
        ;; ---------------- comment out for test begins
        ;; (raise-frame frame)
        ;; (x-focus-frame frame)
        ;; ---------------- end
        ;; (set-mouse-pixel-position frame 4 4)

        ;; default UI in graphic mode
        ;; (xy/mini-gui)
        ;; (xy/tabs-gui)
        (message "Adapt UI for graphical frame."))
    (progn

      ;; Some tweaks for terminal Emacs

      ;; Disable background colors in terminal frames
      ;;
      ;; REF: https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
      ;; (set-face-background 'default "unspecified-bg" frame)
      ;; (set-face-background 'font-lock-comment-face "unspecified-bg" frame)
      (set-face-attribute 'default frame :background "unspecified-bg")
      (set-face-attribute 'font-lock-comment-face frame :background "unspecified-bg")
      (set-face-attribute 'font-lock-comment-delimiter-face frame :background "unspecified-bg")
      (set-face-attribute 'org-block-begin-line frame :overline nil :underline t :extend t)
      (set-face-attribute 'org-block-end-line frame :overline nil :underline t :extend t)

      ;; `hl-line' package
      ;; (custom-set-faces
      ;;  '(hl-line ((t (:background "color-16" :extend t)))))

      ;; -- Try to unify UI-------------------------
      ;; ;; Add padding to emacs frame
      ;; ;; (spacious-padding-mode -1)
      ;; ;; (when (featurep 'spacious-padding) (spacious-padding-mode -1))
      ;; --------------------------------------------

      ;; default UI in text mode
      ;; (xy/mini-gui)
      ;; (xy/tabs-gui)
      (message "Adapt UI config for terminal frame.")))
  (redraw-frame frame))
