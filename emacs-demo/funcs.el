;;; funcs.el --- emacs-demo Layer functions File for Spacemacs
;; Time-stamp: <2023-07-04 Tue 07:03 by xin on tufg>
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

(defun xah-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))
