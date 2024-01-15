;;; eaf-terminal-dedicated.el -*- lexical-binding: t; -*-
;;
;; Authors: Xin Yang <etimecowboy@gmail.com>
;; Package-Version:
;; Package-Commit:
;; Keywords: terminals, processes
;; Version: 1.0
;; Package-Requires: ((emacs "29.0") (eaf "0.0"))
;;
;;; Commentary:
;; Managing dedicated eaf-terminal buffers in Emacs
;; This package is inspired by aweshell.el
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;;  `eaf-pyqterminal'

;;; Code:
(require 'cl-lib)
(require 'eaf-pyqterminal)

(defcustom eaf-pyqterminal-dedicated-window-height 14
  "The height of `pyqterminal' dedicated window."
  :type 'integer
  :group 'eaf-pyqterminal)

(defvar eaf-pyqterminal-dedicated-window nil
  "The dedicated `eaf-pyqterminal' window.")

(defvar eaf-pyqterminal-dedicated-buffer nil
  "The dedicated `eaf-pyqterminal' buffer.")

(defun eaf-current-window-take-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for WINDOW.
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun eaf-pyqterminal-dedicated-exist-p ()
  (and (eaf-buffer-exist-p eaf-pyqterminal-dedicated-buffer)
       (eaf-window-exist-p eaf-pyqterminal-dedicated-window)
       ))

(defun eaf-window-exist-p (window)
  "Return `non-nil' if WINDOW exist.
Otherwise return nil."
  (and window (window-live-p window)))

(defun eaf-buffer-exist-p (buffer)
  "Return `non-nil' if `BUFFER' exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun eaf-get-buffer-names ()
  (let (eaf-buffer-names)
    (dolist (frame (frame-list))
      (dolist (buffer (buffer-list frame))
        (with-current-buffer buffer
          (if (eq major-mode 'eaf-mode)
              (add-to-list 'eaf-buffer-names (buffer-name buffer))))))
    eaf-buffer-names))

(defun eaf-pyqterminal-dedicated-open ()
  "Open dedicated `eaf-pyqterminal' window."
  (interactive)
  (if (eaf-buffer-exist-p eaf-pyqterminal-dedicated-buffer)
      (if (eaf-window-exist-p eaf-pyqterminal-dedicated-window)
          (eaf-pyqterminal-dedicated-select-window)
        (eaf-pyqterminal-dedicated-pop-window))
    (eaf-pyqterminal-dedicated-create-window)))

(defun eaf-pyqterminal-dedicated-close ()
  "Close dedicated `eaf-pyqterminal' window."
  (interactive)
  (if (eaf-pyqterminal-dedicated-exist-p)
      (let ((current-window (selected-window)))
        ;; Remember height.
        (eaf-pyqterminal-dedicated-select-window)
        (delete-window eaf-pyqterminal-dedicated-window)
        (if (eaf-window-exist-p current-window)
            (select-window current-window)))
    (message "`EAF-PYQTERMINAL DEDICATED' window is not exist.")))

(defun eaf-pyqterminal-dedicated-toggle ()
  "Toggle dedicated `eaf-pyqterminal' window."
  (interactive)
  (if (eaf-pyqterminal-dedicated-exist-p)
      (eaf-pyqterminal-dedicated-close)
    (eaf-pyqterminal-dedicated-open)))

(defun eaf-pyqterminal-dedicated-select-window ()
  "Select eaf-pyqterminal dedicated window."
  (select-window eaf-pyqterminal-dedicated-window)
  (set-window-dedicated-p (selected-window) t))

(defun eaf-pyqterminal-dedicated-pop-window ()
  "Pop eaf-pyqterminal dedicated window if it exists."
  (setq eaf-pyqterminal-dedicated-window
        (display-buffer
         (car (eaf-get-buffer-names))
         `(display-buffer-in-side-window
           (side . bottom)
           (window-height . ,eaf-pyqterminal-dedicated-window-height))))
  (select-window eaf-pyqterminal-dedicated-window)
  (set-window-buffer eaf-pyqterminal-dedicated-window
                     eaf-pyqterminal-dedicated-buffer)
  (set-window-dedicated-p (selected-window) t))

(defun eaf-pyqterminal-dedicated-create-window ()
  "Create eaf-pyqterminal dedicated window if it not existing."
  (eaf-open-pyqterminal)
  (setq eaf-pyqterminal-dedicated-buffer (current-buffer))
  (previous-buffer)
  (eaf-pyqterminal-dedicated-pop-window))

(defun eaf-pyqterminal-dedicated-split-window ()
  "Split dedicated window at bottom of frame."
  ;; Select bottom window of frame.
  (ignore-errors
    (dotimes (i 50)
      (windmove-down)))
  ;; Split with dedicated window height.
  (split-window (selected-window)
                (- (eaf-pyqterminal-current-window-take-height)
                   eaf-pyqterminal-dedicated-window-height))
  (other-window 1)
  (setq eaf-pyqterminal-dedicated-window (selected-window)))

(defun eaf-pyqterminal-dedicated-create-buffer ()
  "Create eaf-pyqterminal dedicated buffer."
  (eaf-open-pyqterminal)
  (setq header-line-format nil)
  (setq eaf-pyqterminaly-dedicated-buffer (current-buffer)))

(provide 'eaf-pyqterminal-dedicated)
