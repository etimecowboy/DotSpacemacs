;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- ui Layer functions File for Spacemacs
;; Time-stamp: <2024-01-06 Sat 07:04 by xin on tufg>
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
  (if (not modeline-mode)
      (add-hook 'buffer-list-update-hook #'hidden-mode-line-mode)
    (remove-hook 'buffer-list-update-hook #'hidden-mode-line-mode))
  ;; (force-mode-line-update)
  ;; (redraw-display)
  ;; (revert-buffer)
  )

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
          (set-face-background 'font-lock-comment-face "unspecified-bg"))

        ;; Always use transparent background in new frames
        ;; (set-frame-parameter frame 'alpha-background 80)
        (spacemacs/enable-background-transparency frame)

        ;; `hl-line' package
        (custom-set-faces
         '(hl-line ((t (:background "color-16" :extend t :underline nil)))))

        ;; turn on mode-line
        ;; (spacemacs/toggle-mode-line-on)
        ;; (hidden-mode-line-mode -1)
        ;; (modeline-mode 1)

        ;; turn off header-line
        ;; (xy/turn-off-header-line)
        ;; (path-headerline-mode-off)
        ;; (path-headerline-mode -1)

        ;; Add padding to emacs frame
        ;; (spacious-padding-mode 1)

        ;; Focus on the new frame
        ;;
        ;; REF: https://askubuntu.com/questions/283711/application-focus-of-emacsclient-frame
        ;; ---------------- comment out for test begins
        ;; (raise-frame frame)
        ;; (x-focus-frame frame)
        ;; ---------------- end
        ;; (set-mouse-pixel-position frame 4 4)

        (message "Adapt UI for graphical frame."))
    (progn
      ;; Disable background color in terminal frames
      ;;
      ;; REF: https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
      (set-face-background 'default "unspecified-bg" frame)

      (when (featurep 'spacemacs-theme)
        (set-face-background 'font-lock-comment-face "unspecified-bg"))

      ;; turn off mode line
      ;; (spacemacs/toggle-mode-line-off)
      ;; (hidden-mode-line-mode +1)
      ;; (modeline-mode -1)

      ;; turn on header-line
      ;; (xy/turn-on-header-line)
      ;; (path-headerline-mode-on)
      ;; (path-headerline-mode +1)

      ;; `hl-line' package
      (custom-set-faces
       '(hl-line ((t (:background "color-16" :extend t)))))

      ;; Add padding to emacs frame
      ;; (spacious-padding-mode -1)
      ;; (when (featurep 'spacious-padding) (spacious-padding-mode -1))

      (message "Adapt UI config for terminal frame."))))
