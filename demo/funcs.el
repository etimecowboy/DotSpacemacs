;;; funcs.el --- emacs-demo Layer functions File for Spacemacs
;; Time-stamp: <2024-04-28 Sun 13:40:00 GMT by xin on tufg>
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
  (or state (setq state 1))
  (or scale (setq scale 1.2))
  (or orig (setq orig 0))
  (pcase state
    (`1 (text-scale-set scale))
    (`-1 (text-scale-set orig))
    (_ (error "Invalid state."))))

(defun xy/set-typographic-editing (&optional state)
  "Turn on typographic editing `typo.el' when activated.
Turn off when deactivated."
  (or state (setq state 1))
  (require 'typo nil 'noerror)
  (let ((mm major-mode))
    (when (or (eq mm 'org-mode) (eq mm 'markdown-mode))
      (pcase state
        (`1 (spacemacs/toggle-typographic-substitutions-on))
        (`-1 (spacemacs/toggle-typographic-substitutions-off))
        (_ (error "Invalid state."))))))

;; (defun xy/set-faces (sans-font serif-font mono-font)
;;   "Configure various org-mode faces."
;;   (let ((frame (or frame (selected-frame))))
;;     ;; (when (and (display-graphic-p frame) (eq major-mode 'org-mode))
;;     (when (display-graphic-p frame)
;;       (set-face-attribute 'fixed-pitch frame :family mono-font)
;;       (set-face-attribute 'fixed-pitch-serif frame :family serif-font)
;;       (set-face-attribute 'variable-pitch frame :family sans-font)
;;       ;; (set-face-attribute 'variable-pitch-text frame :height variable-text-height)

;;       ;; (set-face-attribute 'org-default frame :inherit 'default)
;;       ;; (set-face-attribute 'default frame :font default-font) ;; :inherit 'variable-pitch)
;;       (set-face-attribute 'org-default frame :inherit 'variable-pitch)
;;       (set-face-attribute 'org-level-1 frame :inherit '(fixed-pitch-serif bold))
;;       (set-face-attribute 'org-level-2 frame :inherit '(fixed-pitch-serif bold))
;;       (set-face-attribute 'org-level-3 frame :inherit '(fixed-pitch-serif bold))
;;       (set-face-attribute 'org-level-4 frame :inherit '(fixed-pitch-serif bold))
;;       (set-face-attribute 'org-level-5 frame :inherit '(fixed-pitch-serif bold))
;;       (set-face-attribute 'org-level-6 frame :inherit '(fixed-pitch-serif bold))
;;       (set-face-attribute 'org-level-7 frame :inherit '(fixed-pitch-serif bold))
;;       (set-face-attribute 'org-level-8 frame :inherit '(fixed-pitch-serif bold))
;;       (set-face-attribute 'org-document-title frame :inherit '(variable-pitch-text bold))
;;       (set-face-attribute 'org-document-info frame :inherit 'fixed-pitch)
;;       (set-face-attribute 'org-meta-line frame :inherit 'fixed-pitch)
;;       (set-face-attribute 'org-table frame :inherit 'fixed-pitch)
;;       (set-face-attribute 'org-table-header frame :inherit '(fixed-pitch bold))
;;       (set-face-attribute 'org-formula frame :inherit 'fixed-pitch-serif)
;;       (set-face-attribute 'org-code frame :inherit 'fixed-pitch :extend t)
;;       (set-face-attribute 'org-quote frame :inherit '(variable-pitch-text italic))
;;       (set-face-attribute 'org-verbatim frame :inherit 'fixed-pitch)
;;       (set-face-attribute 'org-special-keyword frame :inherit 'fixed-pitch-serif)
;;       (set-face-attribute 'org-checkbox frame :inherit 'fixed-pitch-serif)
;;       (set-face-attribute 'org-date frame :inherit '(fixed-pitch-serif bold))
;;       (set-face-attribute 'org-drawer frame :inherit 'fixed-pitch)
;;       (set-face-attribute 'org-sexp-date frame :inherit 'fixed-pitch)
;;       (set-face-attribute 'org-clock-overlay frame :inherit 'fixed-pitch)
;;       (set-face-attribute 'org-date-selected frame :inherit 'fixed-pitch-serif)
;;       (set-face-attribute 'org-property-value frame :inherit 'fixed-pitch)
;;       (set-face-attribute 'org-block frame :inherit 'fixed-pitch)
;;       (set-face-attribute 'org-block-begin-line frame :inherit '(fixed-pitch-serif bold)
;;                           :overline nil :underline t :extend t)
;;       (set-face-attribute 'org-block-end-line frame :inherit '(fixed-pitch-serif bold)
;;                           :overline t :underline nil :extend t)
;;       )))

(defun xy/demo-fonts ()
  "Reset fonts of some faces to default."
  ;; (xy/set-face-font 'default xy:default-font xy:default-size)
  (xy/set-face-font 'fixed-pitch xy:demo-mono-font xy:demo-mono-size)
  (xy/set-face-font 'fixed-pitch-serif xy:demo-serif-font xy:demo-serif-size)
  (xy/set-face-font 'variable-pitch xy:demo-sans-font xy:demo-sans-size)
  ;; (xy/set-face-font 'variable-pitch-text xy:demo-sans-font xy:variable-pitch-size)
  (xy/set-cjk-font xy:demo-cjk-font xy:demo-cjk-size)
  (redraw-display))

(defun xy/set-demo-fonts (&optional state)
  (interactive)
  (or state (setq state 1))
  (pcase state
    (`1 (xy/demo-fonts))
    (`-1 (xy/reset-fonts))))

(defun xy/set-face-remapping-alist (&optional state)
  "Enable/disable the remapping of buffer local faces."
  (interactive)
  (or state (setq state 1))
  (pcase state
    (`1 (progn
          ;; load org-faces to make sure we can set appropriate faces
          ;; (require 'org-faces)
          (setq-local face-remapping-alist
                      '(;; default
                        ;; (fixed-pitch (:font xy:fixed-pitch-font) fixed-pitch)
                        ;; (fixed-pitch-serif (:font xy:fixed-pitch-serif-font) fixed-pitch-serif)
                        ;; (variable-pitch (:font xy:variable-pitch-font) variable-pitch)
                        ;; (variable-pitch-text (:height xy:variable-pitch-text-height) variable-pitch-text)

                        ;; org mode
                        ;; (org-default (:font xy:demo-default-font) org-default)
                        ;; FIXME: this makes font size growing
                        ;; (default (:height 180) default)
                        ;; font-lock-mode
                        ;; (font-lock-comment-face (:height 0.5) font-lock-comment-face)
                        ;; org-mode: fine-tuning of some faces that are not
                        ;; tuned by org-modern-mode.

                        ;; Last active code
                        ;; (org-document-title (:height 300) org-document-title)
                        ;; (org-level-1 (:height 280) org-level-1)
                        ;; (org-level-2 (:height 260) org-level-2)
                        ;; (org-level-3 (:height 240) org-level-3)
                        ;; (org-level-4 (:height 220) org-level-4)
                        ;; (org-level-5 (:height 200) org-level-5)
                        ;; (org-level-6 (:height 180) org-level-6)
                        ;; (org-level-7 (:height 160) org-level-7)
                        ;; (org-level-8 (:height 140) org-level-8)
                        ;; (org-level-1 (:height 2.0) org-level-1)

                        ;; (org-level-2 (:height 1.8) org-level-2)
                        ;; (org-level-3 (:height 1.6) org-level-3)
                        ;; (org-level-4 (:height 1.4) org-level-4)
                        ;; (org-level-5 (:height 1.3) org-level-5)
                        ;; (org-level-6 (:height 1.2) org-level-6)
                        ;; (org-level-7 (:height 1.1) org-level-7)
                        ;; (org-level-8 (:height 1.0) org-level-8)

                        ;; (org-document-title
                        ;;  (:height 280 :family 'xy:fixed-pitch-serif-font) org-document-title)
                        ;; (org-level-1
                        ;;  (:height 260 :family 'xy:fixed-pitch-serif-font) org-level-1)
                        ;; (org-level-2
                        ;;  (:height 240 :family 'xy:fixed-pitch-serif-font) org-level-2)
                        ;; (org-level-3
                        ;;  (:height 200 :family 'xy:fixed-pitch-serif-font) org-level-3)
                        ;; (org-level-4
                        ;;  (:height 180 :family 'xy:fixed-pitch-serif-font) org-level-4)
                        ;; (org-level-5
                        ;;  (:height 160 :family 'xy:fixed-pitch-serif-font) org-level-5)
                        ;; (org-level-6
                        ;;  (:height 140 :family 'xy:fixed-pitch-serif-font) org-level-6)
                        ;; (org-level-7
                        ;;  (:height 120 :family 'xy:fixed-pitch-serif-font) org-level-7)
                        ;; (org-level-8
                        ;;  (:height 100 :family 'xy:fixed-pitch-serif-font) org-level-8)

                        ;; (org-document-title
                        ;;  (:height 280 :inherent '(xy:fixed-pitch-serif-font bold)) org-document-title)
                        ;; (org-level-1
                        ;;  (:height 260 :inherent '(xy:fixed-pitch-serif-font bold)) org-level-1)
                        ;; (org-level-2
                        ;;  (:height 240 :inherent '(xy:fixed-pitch-serif-font bold)) org-level-2)
                        ;; (org-level-3
                        ;;  (:height 200 :inherent '(xy:fixed-pitch-serif-font bold)) org-level-3)
                        ;; (org-level-4
                        ;;  (:height 180 :inherent '(xy:fixed-pitch-serif-font bold)) org-level-4)
                        ;; (org-level-5
                        ;;  (:height 160 :inherent '(xy:fixed-pitch-serif-font bold)) org-level-5)
                        ;; (org-level-6
                        ;;  (:height 140 :inherent '(xy:fixed-pitch-serif-font bold)) org-level-6)
                        ;; (org-level-7
                        ;;  (:height 120 :inherent '(xy:fixed-pitch-serif-font bold)) org-level-7)
                        ;; (org-level-8
                        ;;  (:height 100 :inherent '(xy:fixed-pitch-serif-font bold)) org-level-8)

                        (org-verbatim (:height 1.1) org-verbatim)
                        (org-code (:height 1.1) org-code)
                        (org-block (:height 1.1) org-block)
                        ))))
    (`-1 (progn
           (setq-local face-remapping-alist
                       '(;; (default (:height 120) default)
                         ;; (fixed-pitch (:font xy:fixed-pitch-font) fixed-pitch)
                         ;; (fixed-pitch-serif (:font xy:fixed-pitch-serif-font) fixed-pitch-serif)
                         ;; (variable-pitch (:font xy:variable-pitch-font) variable-pitch)
                         ;; (variable-pitch-text (:height 1.1) variable-pitch-text)
                         ;; (org-default (:font xy:org-default-font) org-default)

                         ;; Last active code
                         ;; (org-document-title (:height 1.0 :inherent 'bold) org-document-title)
                         ;; (org-level-1 (:height 1.0 :inherent 'bold) org-level-1)
                         ;; (org-level-2 (:height 1.0 :inherent 'bold) org-level-2)
                         ;; (org-level-3 (:height 1.0 :inherent 'bold) org-level-3)
                         ;; (org-level-4 (:height 1.0 :inherent 'bold) org-level-4)
                         ;; (org-level-5 (:height 1.0 :inherent 'bold) org-level-5)
                         ;; (org-level-6 (:height 1.0 :inherent 'bold) org-level-6)
                         ;; (org-level-7 (:height 1.0 :inherent 'bold) org-level-7)
                         ;; (org-level-8 (:height 1.0 :inherent 'bold) org-level-8)
                         ;; (org-verbatim (:height 1.0) org-verbatim)

                         (org-verbatim (:height 1.0) org-verbatim)
                         (org-code (:height 1.0) org-code)
                         (org-block (:height 1.0) org-block)
                         ;; (font-lock-comment-face (:height 1.0) font-lock-comment-face)
                         ;; (load-theme 'spacemacs-dark t)
                         ))))
    ;; (setq-local face-remapping-alist '((default variable-pitch default)))
    ;; (spacemacs//set-monospaced-font "Sarasa Mono SC Nerd Font" "Sarasa Mono SC Nerd Font" 16 16)
    ;; (spacemacs//set-monospaced-font "FiraCode Nerd Font" "BabelStone Han" 14 18)
    (_ (error "Invalid state."))))

;; (defun xy/set-face-remapping-alist (&optional state)
;;   "Enable/disable the remapping of buffer local faces."
;;   (interactive)
;;   (let ((state (or state 1)))
;;     (pcase state
;;       (`1 (progn
;;             ;; load org-faces to make sure we can set appropriate faces
;;             ;; (require 'org-faces)
;;             (setq-local face-remapping-alist
;;                         '(;; default
;;                           ;; (fixed-pitch (:family xy:demo-mono-font) fixed-pitch)
;;                           ;; (fixed-pitch-serif (:family xy:demo-serif-font) fixed-pitch-serif)
;;                           ;; (variable-pitch (:family xy:demo-sans-font) variable-pitch)
;;                           ;; (variable-pitch-text (:height xy:variable-pitch-text-height) variable-pitch-text)
;;                           ;; (org-default (:inherent 'variable-pitch) org-default)
;;                           ;; FIXME: this makes font size growing
;;                           ;; (default (:height 180) default)
;;                           ;; font-lock-mode
;;                           ;; (font-lock-comment-face (:height 0.5) font-lock-comment-face)
;;                           ;; org-mode: fine-tuning of some faces that are not
;;                           ;; tuned by org-modern-mode.
;;
;;                           (org-document-title (:family xy:demo-sans-font :height 2.5) org-document-title)
;;                           (org-level-1 (:family xy:demo-sans-font :height 2.0) org-level-1)
;;                           (org-level-2 (:family xy:demo-sans-font :height 1.8) org-level-2)
;;                           (org-level-3 (:family xy:demo-sans-font :height 1.6) org-level-3)
;;                           (org-level-4 (:family xy:demo-sans-font :height 1.4) org-level-4)
;;                           (org-level-5 (:family xy:demo-sans-font :height 1.3) org-level-5)
;;                           (org-level-6 (:family xy:demo-sans-font :height 1.2) org-level-6)
;;                           (org-level-7 (:family xy:demo-sans-font :height 1.1) org-level-7)
;;                           (org-level-8 (:family xy:demo-sans-font :height 1.0) org-level-8)
;;                           (org-verbatim (:family xy:demo-sans-font :height 1.2) org-verbatim)
;;                           (org-code (:family xy:demo-mono-font :height 1.2) org-code)
;;                           (org-block (:family xy:demo-mono-font :height 1.2) org-block)
;;                           ))))
;;       (`-1 (progn
;;              (setq-local face-remapping-alist
;;                          '(;; (default (:height 120) default)
;;                            ;; (fixed-pitch (:font xy:fixed-pitch-font) fixed-pitch)
;;                            ;; (fixed-pitch-serif (:font xy:fixed-pitch-serif-font) fixed-pitch-serif)
;;                            ;; (variable-pitch (:font xy:variable-pitch-font) variable-pitch)
;;                            ;; (variable-pitch-text (:height 1.1) variable-pitch-text)
;;                            ;; (org-default (:font xy:org-default-font) org-default)
;;                            ;; (fixed-pitch (:family xy:fixed-pitch-font) fixed-pitch)
;;                            ;; (fixed-pitch-serif (:family xy:fixed-pitch-serif-font) fixed-pitch-serif)
;;                            ;; (variable-pitch (:family xy:variable-pitch-font) variable-pitch)
;;                            ;; (variable-pitch-text (:height xy:variable-pitch-text-height) variable-pitch-text)
;;                            ;; (org-default (:inherent 'variable-pitch) org-default)
;;
;;                            (org-document-title (:family 'unspecified :height 1.0) org-document-title)
;;                            (org-level-1 (:family 'unspecified :height 1.0) org-level-1)
;;                            (org-level-2 (:family 'unspecified :height 1.0) org-level-2)
;;                            (org-level-3 (:family 'unspecified :height 1.0) org-level-3)
;;                            (org-level-4 (:family 'unspecified :height 1.0) org-level-4)
;;                            (org-level-5 (:family 'unspecified :height 1.0) org-level-5)
;;                            (org-level-6 (:family 'unspecified :height 1.0) org-level-6)
;;                            (org-level-7 (:family 'unspecified :height 1.0) org-level-7)
;;                            (org-level-8 (:family 'unspecified :height 1.0) org-level-8)
;;                            (org-verbatim (:family 'unspecified :height 1.0) org-verbatim)
;;                            (org-code (:height 1.0) org-code)
;;                            (org-block (:height 1.0) org-block)
;;                            ;; (font-lock-comment-face (:height 1.0) font-lock-comment-face)
;;                            ;; (load-theme 'spacemacs-dark t)
;;                            ))))
;;       ;; (setq-local face-remapping-alist '((default variable-pitch default)))
;;       ;; (spacemacs//set-monospaced-font "Sarasa Mono SC Nerd Font" "Sarasa Mono SC Nerd Font" 16 16)
;;       ;; (spacemacs//set-monospaced-font "FiraCode Nerd Font" "BabelStone Han" 14 18)
;;       (_ (error "Invalid state.")))))

(defun xy/set-hide-emphasis-markers (&optional state)
  "Hide/show emphasis-marksers in current org buffer."
  (interactive)
  (or state (setq state 1))
  (when (eq major-mode 'org-mode)
    (pcase state
      (`1 (progn
            ;; Hide emphasis markers on formatted text
            (setq-local org-hide-emphasis-markers t)
            ))
      (`-1 (progn
             ;; Display emphasis markers on formatted text
             (setq-local org-hide-emphasis-markers nil)
             ))
      (_ (error "Invalid state.")))))

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

(defun xy/toggle-org-demo ()
  "Toggle org-mode demo/presentation."
  (interactive)
  (require 'spacemacs-theme)
  (require 'writeroom-mode)
  (require 'command-log-mode)
  (require 'org-tree-slide)
  (let ((mm major-mode))
    (when (eq mm 'org-mode)
      (save-buffer)
      (if org-tree-slide-mode
          (progn
            ;; NOTE: close the log buffer no matter it exists or not
            (clm/close-command-log-buffer)
            (global-command-log-mode -1)
            (focus-read-only-mode -1)
            (focus-mode -1)
            (org-tree-slide-mode -1)
            (load-theme 'spacemacs-dark t)
            (writeroom-mode -1)
            (org-fragtog-mode 1)
            (xy/reset-fonts)
            (revert-buffer t t)
            (org-fold-show-all)
            (org-clear-latex-preview)
            (org--latex-preview-region (point-min) (point-max))
            (org-latex-preview 16) ;; redisplay preview images
            )
        (progn
          (global-hl-line-mode -1)
          (global-command-log-mode 1)
          ;; NOTE: don't open the log buffer by default
          ;; (clm/open-command-log-buffer)
          (org-fragtog-mode -1)
          (xy/demo-fonts)
          (setq-default org-hide-emphasis-markers t)
          (setq-local org-hide-emphasis-markers t)
          (load-theme 'spacemacs-light t)
          (revert-buffer t t)
          (org-fold-show-all)
          (org-clear-latex-preview)
          (org--latex-preview-region (point-min) (point-max))
          (org-fragtog-mode -1)
          (writeroom-mode 1)
          (org-tree-slide-mode 1)
          (focus-mode 1)
          (focus-read-only-mode 1)
          )))))

