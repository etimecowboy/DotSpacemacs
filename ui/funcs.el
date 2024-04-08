;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- ui Layer functions File for Spacemacs
;; Time-stamp: <2024-04-06 Sat 13:01 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

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

(defun xy/toggle-my-focus ()
  "Toggle focused working buffer."
  (interactive)
  (require 'writeroom-mode)
  (require 'face-remap)
  (unless (eq major-mode 'treemacs-mode)
    (if writeroom-mode
        (progn
          (writeroom-mode -1)
          (text-scale-set 0))
      (progn
        (writeroom-mode 1)
        (text-scale-set 2))
      (force-mode-line-update t))))

(defun xy/toggle-breadcrumb ()
  "Toggle breadcrumb header line in current buffer."
  (interactive)
  (require 'breadcrumb)
  (if breadcrumb-local-mode
      (progn
        (when (featurep 'tab-line) (tab-line-mode 1))
        (breadcrumb-local-mode -1))
    (progn
      (when (featurep 'tab-line) (tab-line-mode -1))
      (breadcrumb-local-mode 1)))
  (force-mode-line-update t))

(defun xy/toggle-global-breadcrumb ()
  "Toggle breadcrumb header line globally."
  (interactive)
  (require 'breadcrumb)
  (if (or breadcrumb-local-mode breadcrumb-mode)
      (progn
        (when (featurep 'tab-line) (global-tab-line-mode 1))
        (breadcrumb-mode -1))
    (progn
      (when (featurep 'tab-line) (global-tab-line-mode -1))
      (breadcrumb-mode 1)))
  (force-mode-line-update t))

(defun xy/toggle-tabs ()
  "Toggle all tabs in current window."
  (interactive)
  (require 'tab-bar)
  (require 'tab-line)
  (xy/tabbar-setup)
  (toggle-frame-tab-bar)
  (if tab-line-mode
      (tab-line-mode -1)
    (progn
      (tab-line-mode 1)
      (xy/tabline-setup)))
  (when (featurep 'breadcrumb)
    (breadcrumb-local-mode -1))
  ;; Force a mode-line update because it updates the tab line as well.
  (force-mode-line-update t))

(defun xy/toggle-global-tabs ()
  "Toggle all tabs globally."
  (interactive)
  (require 'tab-bar)
  (require 'tab-line)
  (if tab-bar-mode
      (tab-bar-mode -1)
    (progn
      (tab-bar-mode 1)
      (xy/tabbar-setup)))
  (if (and global-tab-line-mode tab-line-mode)
      (global-tab-line-mode -1)
    (progn
      (global-tab-line-mode 1)
      (xy/tabline-setup)))
  (when (featurep 'breadcrumb)
    (breadcrumb-mode -1))
  ;; Force a mode-line update because it updates the tab line as well.
  (force-mode-line-update t))

(defun xy/turn-on-global-tabs ()
  "Turn on all tabs globally."
  (interactive)
  (require 'tab-bar)
  (require 'tab-line)
  (modeline-mode -1)
  (spacemacs/toggle-mode-line-off)
  (tab-bar-mode 1)
  (xy/tabbar-setup)
  (global-tab-line-mode 1)
  (xy/tabline-setup)
  (when (featurep 'breadcrumb) (breadcrumb-mode -1))
  (force-mode-line-update t))

(defun xy/turn-off-global-tabs ()
  "Turn off all tabs globally."
  (interactive)
  (require 'tab-bar)
  (require 'tab-line)
  (tab-bar-mode -1)
  (global-tab-line-mode -1)
  (force-mode-line-update t))

(defun xy/ide-gui ()
  "Turn on IDE GUI."
  (interactive)
  (require 'treemacs)
  (modeline-mode -1)
  (spacemacs/toggle-mode-line-off)
  (xy/turn-on-global-tabs)
  (treemacs-select-window)
  (treemacs-select-window)
  (force-mode-line-update t))

3(defun xy/tabs-gui ()
  "Turn on tabs GUI."
  (interactive)
  (spacemacs/toggle-mode-line-off)
  (xy/turn-on-global-tabs)
  (when (featurep 'treemacs)
    (treemacs-select-window)
    (treemacs-quite))
  (force-mode-line-update t))

(defun xy/default-gui ()
  "Turn on default GUI."
  (interactive)
  (xy/turn-off-global-tabs)
  (when (featurep 'breadcrumb) (breadcrumb-mode -1))
  (when (featurep 'treemacs)
    (treemacs-select-window)
    (treemacs-quit))
  (modeline-mode 1)
  (spacemacs/toggle-mode-line-on)
  (force-mode-line-update t)
  (redraw-display)
  ;; (save-buffer)
  ;; (revert-buffer t t)
  )

(defun xy/mini-gui ()
  "Turn on mini GUI."
  (interactive)
  (require 'breadcrumb)
  (xy/turn-off-global-tabs)
  (breadcrumb-mode 1)
  (when (featurep 'treemacs)
    (treemacs-select-window)
    (treemacs-quit))
  (modeline-mode -1)
  (spacemacs/toggle-mode-line-off)
  (force-mode-line-update t))

(defun xy/adapt-ui-config (&optional frame)
  "Adapt UI to work in terminal or graphical environment."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (if (display-graphic-p frame)
      (progn
        ;; color-theme
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

        (when (featurep 'spacemacs-theme)
          (set-face-background 'font-lock-comment-face "unspecified-bg")
          ;; (custom-set-faces
          ;;  '(tab-line ((t (:background "dark violet"
          ;;                  :foreground "white"
          ;;                  :family "Sarasa Mono SC Nerd Font"
          ;;                  ))))
          ;;  '(tab-line-highlight ((t (:inherit tab-line
          ;;                            :background "magenta"
          ;;                            :foreground "#b2b2b2"
          ;;                            ))))
          ;;  '(tab-line-tab ((t (:inherit tab-line-highlight
          ;;                      :box (:style released-button)
          ;;                      ))))
          ;;  '(tab-line-tab-current ((t (:inherit tab-line-tab
          ;;                              :background "magenta"
          ;;                              :weight bold
          ;;                              :box (:style pressed-button)
          ;;                              ))))
          ;;  '(tab-line-tab-inactive ((t (:inherit tab-line-tab
          ;;                               :background "purple"
          ;;                               :foreground "gray60"
          ;;                               ))))
          ;;  '(tab-line-tab-modified ((t (:inherit tab-line-tab
          ;;                               :foreground "yellow"
          ;;                              ))))
          ;;  '(tab-bar ((t (:background "dark violet"
          ;;                 :foreground "white"
          ;;                 :family "Sarasa Mono SC Nerd Font"
          ;;                 ))))
          ;;  '(tab-bar-tab ((t (:inherit tab-bar
          ;;                     :background "magenta"
          ;;                     :box (:style released-button)))))
          ;;  '(tab-bar-tab-group-current ((t (:inherit tab-bar-tab
          ;;                                   :background "magenta"
          ;;                                   :weight bold
          ;;                                   :box (:style pressed-button)))))
          ;;  '(tab-bar-tab-group-inactive ((t (:inherit tab-bar-tab
          ;;                                    :background "purple"
          ;;                                    :foreground "gray60"))))
          ;;  '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab
          ;;                              :background "purple"
          ;;                              :foreground "gray60"))))
          ;;  '(tab-bar-tab-ungrouped ((t (:inherit tab-bar-tab
          ;;                               :background "purple"
          ;;                               :foreground "gray60")))))
          )

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

        ;; Always use transparent background in new frames
        ;; (set-frame-parameter frame 'alpha-background 80)
        (spacemacs/enable-background-transparency frame)

        ;; `hl-line' package
        (custom-set-faces
         '(hl-line ((t (:background "color-16" :extend t :underline nil)))))

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

        (xy/tabs-gui)
        (message "Adapt UI for graphical frame."))
    (progn

      (when (featurep 'spacemacs-theme)
        (set-face-background 'font-lock-comment-face "unspecified-bg"))

      ;; Disable background color in terminal frames
      ;;
      ;; REF: https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
      (set-face-background 'default "unspecified-bg" frame)

      ;; `hl-line' package
      (custom-set-faces
       '(hl-line ((t (:background "color-16" :extend t)))))

      ;; -- Try to unify UI-------------------------
      ;; ;; turn off mode line
      ;; ;; (spacemacs/toggle-mode-line-off)
      ;; ;; (hidden-mode-line-mode +1)
      ;; (modeline-mode -1)
      ;;
      ;; ;; turn on header-line
      ;; ;; (xy/turn-on-header-line)
      ;; ;; (path-headerline-mode-on)
      ;; ;; (path-headerline-mode +1)
      ;; (breadcrumb-mode 1)
      ;;
      ;; ;; Add padding to emacs frame
      ;; ;; (spacious-padding-mode -1)
      ;; ;; (when (featurep 'spacious-padding) (spacious-padding-mode -1))
      ;; --------------------------------------------

      (xy/tabs-gui)
      (message "Adapt UI config for terminal frame.")))
  (redraw-frame frame))
