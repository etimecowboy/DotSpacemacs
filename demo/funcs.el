;;; funcs.el --- emacs-demo Layer functions File for Spacemacs
;; Time-stamp: <2023-07-11 Tue 10:15 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; (defun xy/prepare-emacs-demo (&optional frame)
;;   (interactive)
;;   "Adapt emacs to work in terminal or graphical environment."
;;   (or frame (setq frame (selected-frame)))
;;   (if (display-graphic-p frame)
;;       (progn
;;         (set-frame-parameter frame 'alpha-background 70)
;;         (keycast-header-line-mode 1)
;;         )
;;     (progn
;;       (set-face-background 'default "unspecified-bg" frame)
;;       )))

;; REF: http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html
(defun xy/toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))

(defun xy/set-line-spacing (&optional sp)
  "Prompt user to set the line spacing."
  (interactive)
  (if sp
      (setq line-spacing sp)
    (let (n)
      (setq n (read-number "Set line spacing to: " 1.0))
      (setq line-spacing n)))
  (redraw-frame (selected-frame)))

;; Test resutls
;; (xy/toggle-line-spacing)
;; (xy/toggle-line-spacing)
;; (xy/set-line-spacing 0.8)
;; (xy/set-line-spacing)
;; (xy/toggle-line-spacing)
;; (xy/toggle-line-spacing)

