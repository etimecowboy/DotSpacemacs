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
;;  `eaf-terminal'

;;; Code:
(require 'cl-lib)
(require 'eaf-terminal)

(defvar eaf-terminal-dedicated-window nil
  "The dedicated `eaf-terminal' window.")

(defvar eaf-terminal-dedicated-buffer nil
  "The dedicated `eaf-terminal' buffer.")

(defcustom eaf-terminal-dedicated-window-height 14
  "The height of `eaf-terminal' dedicated window."
  :type 'integer
  :group 'eaf)

(defun eaf-terminal-get-buffer-index ()
  (let ((eaf-terminal-buffer-index-list (eaf-terminal-get-buffer-index-list))
        (eaf-terminal-buffer-index-counter 1))
    (if eaf-terminal-buffer-index-list
        (progn
          (dolist (buffer-index eaf-terminal-buffer-index-list)
            (if (equal buffer-index eaf-terminal-buffer-index-counter)
                (setq eaf-terminal-buffer-index-counter (+ 1 eaf-terminal-buffer-index-counter))
              (return eaf-terminal-buffer-index-counter)))
          eaf-terminal-buffer-index-counter)
      1)))

(defun eaf-terminal-get-buffer-names ()
  (let (eaf-terminal-buffer-names)
    (dolist (frame (frame-list))
      (dolist (buffer (buffer-list frame))
        (with-current-buffer buffer
          (if (eq major-mode 'eaf-terminal-mode)
              (add-to-list 'eaf-terminal-buffer-names (buffer-name buffer))))))
    eaf-terminal-buffer-names))

(defun eaf-terminal-get-buffer-index-list ()
  (let ((eaf-terminal-buffer-names (eaf-terminal-get-buffer-names)))
    (if eaf-terminal-buffer-names
        (let* ((eaf-terminal-buffer-index-strings
                (seq-filter (function
                             (lambda (buffer-index)
                               (and (stringp buffer-index)
                                    (not (equal 0 (string-to-number buffer-index))))))
                            (mapcar (function
                                     (lambda (buffer-name)
                                       (if (integerp (string-match "\\*eaf-terminal\\*\<\\([0-9]+\\)\>" buffer-name))
                                           (subseq buffer-name (match-beginning 1) (match-end 1))
                                         nil)))
                                    eaf-terminal-buffer-names)))
               (eaf-terminal-buffer-index-list (sort (seq-map 'string-to-number eaf-terminal-buffer-index-strings) '<)))
          eaf-terminal-buffer-index-list)
      nil)))


(defun eaf-terminal-current-window-take-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for WINDOW.
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun eaf-terminal-dedicated-exist-p ()
  (and (eaf-terminal-buffer-exist-p eaf-terminal-dedicated-buffer)
       (eaf-terminal-window-exist-p eaf-terminal-dedicated-window)
       ))

(defun eaf-terminal-window-exist-p (window)
  "Return `non-nil' if WINDOW exist.
Otherwise return nil."
  (and window (window-live-p window)))

(defun eaf-terminal-buffer-exist-p (buffer)
  "Return `non-nil' if `BUFFER' exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun eaf-terminal-dedicated-open ()
  "Open dedicated `eaf-terminal' window."
  (interactive)
  (if (eaf-terminal-buffer-exist-p eaf-terminal-dedicated-buffer)
      (if (eaf-terminal-window-exist-p eaf-terminal-dedicated-window)
          (eaf-terminal-dedicated-select-window)
        (eaf-terminal-dedicated-pop-window))
    (eaf-terminal-dedicated-create-window)))

(defun eaf-terminal-dedicated-close ()
  "Close dedicated `eaf-terminal' window."
  (interactive)
  (if (eaf-terminal-dedicated-exist-p)
      (let ((current-window (selected-window)))
        ;; Remember height.
        (eaf-terminal-dedicated-select-window)
        (delete-window eaf-terminal-dedicated-window)
        (if (eaf-terminal-window-exist-p current-window)
            (select-window current-window)))
    (message "`EAF-TERMINAL DEDICATED' window is not exist.")))

(defun eaf-terminal-dedicated-toggle ()
  "Toggle dedicated `eaf-terminal' window."
  (interactive)
  (if (eaf-terminal-dedicated-exist-p)
      (eaf-terminal-dedicated-close)
    (eaf-terminal-dedicated-open)))

(defun eaf-terminal-dedicated-select-window ()
  "Select eaf-terminal dedicated window."
  (select-window eaf-terminal-dedicated-window)
  (set-window-dedicated-p (selected-window) t))

(defun eaf-terminal-dedicated-pop-window ()
  "Pop eaf-terminal dedicated window if it exists."
  (setq eaf-terminal-dedicated-window (display-buffer (car (eaf-terminal-get-buffer-names)) `(display-buffer-in-side-window (side . bottom) (window-height . ,eaf-terminal-dedicated-window-height))))
  (select-window eaf-terminal-dedicated-window)
  (set-window-buffer eaf-terminal-dedicated-window eaf-terminal-dedicated-buffer)
  (set-window-dedicated-p (selected-window) t))

(defun eaf-terminal-dedicated-create-window ()
  "Create eaf-terminal dedicated window if it not existing."
  (eaf-open-terminal)
  (setq eaf-terminal-dedicated-buffer (current-buffer))
  (previous-buffer)
  (eaf-terminal-dedicated-pop-window))

(defun eaf-terminal-dedicated-split-window ()
  "Split dedicated window at bottom of frame."
  ;; Select bottom window of frame.
  (ignore-errors
    (dotimes (i 50)
      (windmove-down)))
  ;; Split with dedicated window height.
  (split-window (selected-window) (- (eaf-terminal-current-window-take-height) eaf-terminal-dedicated-window-height))
  (other-window 1)
  (setq eaf-terminal-dedicated-window (selected-window)))

(defun eaf-terminal-dedicated-create-buffer ()
  "Create eaf-terminal dedicated buffer."
  (eaf-open-terminal)
  (setq header-line-format nil)
  (setq eaf-terminal-dedicated-buffer (current-buffer)))

;; (defadvice delete-other-windows (around eaf-terminal-delete-other-window-advice activate)
;;   "This is advice to make `eaf-terminal' avoid dedicated window deleted.
;; Dedicated window can't deleted by command `delete-other-windows'."
;;   (unless (eq (selected-window) eaf-terminal-dedicated-window)
;;     (let ((eaf-terminal-dedicated-active-p (eaf-terminal-window-exist-p eaf-terminal-dedicated-window)))
;;       (if eaf-terminal-dedicated-active-p
;;           (let ((current-window (selected-window)))
;;             (cl-dolist (win (window-list))
;;               (when (and (window-live-p win)
;;                          (not (eq current-window win))
;;                          (not (window-dedicated-p win)))
;;                 (delete-window win))))
;;         ad-do-it))))

;; (defadvice other-window (after eaf-terminal-dedicated-other-window-advice)
;;   "Default, can use `other-window' select window in cyclic ordering of windows.
;; But sometimes we don't want to select `sr-speedbar' window,
;; but use `other-window' and just make `eaf-terminal' dedicated
;; window as a viewable sidebar.

;; This advice can make `other-window' skip `eaf-terminal' dedicated window."
;;   (let ((count (or (ad-get-arg 0) 1)))
;;     (when (and (eaf-terminal-window-exist-p eaf-terminal-dedicated-window)
;;                (eq eaf-terminal-dedicated-window (selected-window)))
;;       (other-window count))))

(provide 'eaf-terminal-dedicated)
