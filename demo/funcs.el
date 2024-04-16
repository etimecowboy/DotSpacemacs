;;; funcs.el --- emacs-demo Layer functions File for Spacemacs
;; Time-stamp: <2024-04-15 Mon 18:17 by xin on tufg>
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

;; ;; REF: http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html
;; (defun xy/toggle-line-spacing ()
;;   "Toggle line spacing between no extra space to extra half line height."
;;   (interactive)
;;   (if line-spacing
;;       (setq line-spacing nil)
;;     (setq line-spacing 0.5))
;;   (redraw-frame (selected-frame)))

;; (defun xy/set-line-spacing (&optional sp)
;;   "Prompt user to set the line spacing."
;;   (interactive)
;;   (if sp
;;       (setq line-spacing sp)
;;     (let (n)
;;       (setq n (read-number "Set line spacing to: " 1.0))
;;       (setq line-spacing n)))
;;   (redraw-frame (selected-frame)))

;; (defun xy/toggle-org-tree-slide ()
;;   "Toggle org-tree-slide mode in org-mode."
;;   (interactive)
;;   (require 'org-tree-slide)
;;   (if (eq major-mode 'org-mode)
;;       (progn
;;         (if org-tree-slide-mode
;;             (org-tree-slide-mode -1)
;;           (org-tree-slide-mode 1)))
;;     (message "Current buffer is not org-mode.")))

;; (defun xy/toggle-focus ()
;;   "Toggle focused working window."
;;   (interactive)
;;   (require 'writeroom-mode)
;;   (require 'face-remap)
;;   (unless (eq major-mode 'treemacs-mode)
;;     (if writeroom-mode
;;         (progn
;;           ;; (xy/tabs-gui)
;;           (writeroom-mode -1)
;;           (text-scale-set 0))
;;       (progn
;;         ;; (xy/mini-gui)
;;         (writeroom-mode 1)
;;         (text-scale-set 2))
;;       (force-mode-line-update t))))

;; (defun xy/toggle-demo ()
;;   "Start demonstration in org mode"
;;   (interactive)
;;   (xy/mini-gui)
;;   (xy/toggle-focus)
;;   (if writeroom-mode
;;       (progn
;;         (when (eq major-mode 'org-mode) (org-tree-slide-mode 1))
;;         (global-command-log-mode 1)
;;         (clm/open-command-log-buffer))
;;     (progn
;;       (when (eq major-mode 'org-mode) (org-tree-slide-mode -1))
;;       (global-command-log-mode -1)
;;       (clm/close-command-log-buffer))))

;; (defun xy/toggle-demo ()
;;   "Start demonstration in org mode"
;;   (interactive)
;;   (xy/mini-gui)
;;   (xy/toggle-focus)
;;   (if writeroom-mode
;;       (progn
;;         (when (eq major-mode 'org-mode) (org-tree-slide-mode 1))
;;         (global-command-log-mode 1)
;;         (clm/open-command-log-buffer))
;;     (progn
;;       (when (eq major-mode 'org-mode) (org-tree-slide-mode -1))
;;       (global-command-log-mode -1)
;;       (clm/close-command-log-buffer))))

;; Test resutls
;; (xy/toggle-line-spacing)
;; (xy/toggle-line-spacing)
;; (xy/set-line-spacing 0.8)
;; (xy/set-line-spacing)
;; (xy/toggle-line-spacing)
;; (xy/toggle-line-spacing)

(defun xy/set-buffer-text-scale (&optional state scale orig)
  "Scale buffer text bigger when activated. Scale text buffer
 back when deactivated."
  (let ((state (or state 1))
        (scale (or scale 2))
        (orig (or scale 0)))
    (pcase state
      (`1 (text-scale-set scale))
      (`-1 (text-scale-set orig))
      (_ (error "Invalid state.")))))

(defun xy/set-variable-pitch (&optional state)
  "Enable/disable `variable-pitch-mode' for specific major modes."
  (interactive)
  (let ((state (or state 1))
        (mm major-mode))
    (when (or (eq mm 'org-mode) (eq mm 'text-mode))
      (pcase state
        (`1 (progn
              (variable-pitch-mode 1)))
        (`-1 (progn
               (variable-pitch-mode -1)))
        (_ (error "Invalid state."))))))


(defun xy/set-face-remapping-alist (&optional state)
  "Enable/disable the remapping of buffer local faces."
  (let ((state (or state 1)))
    (pcase state
      (`1 (progn
            ;; load org-faces to make sure we can set appropriate faces
            ;; (require 'org-faces)
            (setq-local face-remapping-alist
                        '(;; default
                          ;; FIXME: this makes font size growing
                          ;; (default (:height 180) default)
                          ;; font-lock-mode
                          ;; (font-lock-comment-face (:height 0.5) font-lock-comment-face)
                          ;; org-mode: fine-tuning of some faces that are not
                          ;; tuned by org-modern-mode.
                          (org-document-title (:height 2.0) org-document-title)
                          (org-verbatim (:height 1.5) org-verbatim)
                          ;; (org-code (:height 0.8) org-code)
                          ))))
      (`-1 (progn
             (setq-local face-remapping-alist
                         '(;; (default (:height 120) default)
                           (org-document-title (:height 1.0) org-document-title)
                           (org-verbatim (:height 1.0) org-verbatim)
                           ;; (org-code (:height 1.0) org-code)
                           ;; (font-lock-comment-face (:height 1.0) font-lock-comment-face)
                           ))))
      ;; (setq-local face-remapping-alist '((default variable-pitch default)))
      ;; (spacemacs//set-monospaced-font "Sarasa Mono SC Nerd Font" "Sarasa Mono SC Nerd Font" 16 16)
      ;; (spacemacs//set-monospaced-font "FiraCode Nerd Font" "BabelStone Han" 14 18)
      (_ (error "Invalid state.")))))


(defun xy/set-hide-emphasis-markers (&optional state)
  "Hide/show org-mode emphasis-marksers."
  (interactive)
  (let ((state (or state 1)))
    (when (eq major-mode 'org-mode)
      (pcase state
        (`1 (progn
              ;; Hide emphasis markers on formatted text
              (setq org-hide-emphasis-markers t)
              ;; (org-mode-restart)
              (save-buffer)
              (revert-buffer t t)
              ;;(writeroom-mode 1)
              ;;(unless writeroom-mode (writeroom-mode 1))
              ))
        (`-1 (progn
               ;; Display emphasis markers on formatted text
               (setq org-hide-emphasis-markers nil)
               ;; (org-mode-restart)
               (save-buffer)
               (revert-buffer t t)
               ;;(when writeroom-mode (writeroom-mode -1))
               ))
        (_ (error "Invalid state."))))))

;; ;; Set reusable font name variables
;; (defvar xy:demo-fixed-width-font  "Sarasa Mono SC Nerd Font" ;; "FiraCode Nerd Font"
;;   "The font to use for monospaced (fixed width) text.")

;; (defvar xy:demo-variable-width-font "Times New Roman"
;;   "The font to use for variable-pitch (document) text.")

;; (set-face-attribute 'default nil
;;                     :font xy:demo-fixed-width-font
;;                     ;; :weight 'light
;;                     :height 120
;;                     )
;; (set-face-attribute 'fixed-pitch nil
;;                     :font xy:demo-fixed-width-font
;;                     ;; :weight 'light
;;                     ;; :height 1.5
;;                     )
;; (set-face-attribute 'variable-pitch nil
;;                     :font xy:demo-variable-width-font
;;                     ;; :weight 'light
;;                     ;; :height 1.5
;;                     )
;; (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; (defun xy/set-face-remapping-alist (&optional state)
;;   "Set effects for writeroom-mode."
;;   (let ((state (or state 1)))
;;     (pcase state
;;       (`1 (progn
;;             ;; load org-faces to make sure we can set appropriate faces
;;             (require 'org-faces)
;;             (setq-local face-remapping-alist
;;                         '(;; (default variable-pitch)
;;                           ;; (org-document-title org-document-title)
;;                           (org-table fixed-pitch)
;;                           (org-code fixed-pitch)
;;                           (org-block fixed-pitch)
;;                           (org-verbatim fixed-pitch)
;;                           (org-block-begin-line fixed-pitch)
;;                           )))
;;           )
;;       (`-1 (progn
;;              (setq-local face-remapping-alist '((default variable-pitch default)))
;;              ;; (spacemacs//set-monospaced-font "Sarasa Mono SC Nerd Font" "Sarasa Mono SC Nerd Font" 16 16)
;;              ;; (spacemacs//set-monospaced-font "FiraCode Nerd Font" "BabelStone Han" 14 18)
;;              ))
;;       (_ (error "Invalid state.")))))
