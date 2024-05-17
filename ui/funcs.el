;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- ui Layer functions File for Spacemacs
;; Time-stamp: <2024-05-06 Mon 02:44:32 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; Change loaded color themes
;; REF: https://github.com/lepisma/rogue/blob/master/config.el
(require 'color)
(defun color-12-to-6 (color-str)
  "Convert 12 char color representation to 6 char"
  (if (= 7 (length color-str))
      color-str
    (apply #'color-rgb-to-hex `(,@(color-name-to-rgb color-str) 2))))
(defmacro r|set-pair-faces (themes consts faces-alist)
  "Macro for pair setting of custom faces.
THEMES name the pair (theme-one theme-two). CONSTS sets the variables like
  ((sans-font \"Some Sans Font\") ...). FACES-ALIST has the actual faces
like:
  ((face1 theme-one-attr theme-two-atrr)
   (face2 theme-one-attr nil           )
   (face3 nil            theme-two-attr)
   ...)
TODO: Simplify this macro"
  (defmacro r||get-proper-faces ()
    `(let* (,@consts)
       (backquote ,faces-alist)))

  `(setq theming-modifications
         ',(mapcar (lambda (theme)
                     `(,theme ,@(cl-remove-if
                                 (lambda (x) (equal x "NA"))
                                 (mapcar (lambda (face)
                                           (let ((face-name (car face))
                                                 (face-attrs (nth (cl-position theme themes) (cdr face))))
                                             (if face-attrs
                                                 `(,face-name ,@face-attrs)
                                               "NA"))) (r||get-proper-faces)))))
                   themes)))

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
        (xy/tabbar-setup)
        (tab-line-mode 1)
        (global-tab-line-mode 1)
        (xy/tabline-setup))
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
  (spacemacs/toggle-mode-line-off)
  (breadcrumb-mode 1)
  (global-tabs-mode 1)
  (xy/open-treemacs-window)
  (force-mode-line-update t))

(defun xy/tabs-gui ()
  "Turn on tabs GUI."
  (interactive)
  (modeline-mode -1)
  (breadcrumb-mode -1)
  (spacemacs/toggle-mode-line-off)
  (global-tabs-mode 1)
  (xy/close-treemacs-window)
  (force-mode-line-update t))

(defun xy/default-gui ()
  "Turn on default GUI."
  (interactive)
  (modeline-mode 1)
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
  (spacemacs/toggle-mode-line-off)
  (breadcrumb-mode 1)
  (global-tabs-mode -1)
  (xy/close-treemacs-window)
  (force-mode-line-update t))

(defun xy/adapt-ui-config (&optional frame)
  "Adapt UI to work in terminal or graphical environment."
  (interactive)
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

        (when (featurep 'spacemacs-theme)
          ;; (set-face-background 'font-lock-comment-face "unspecified-bg")
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

          ;; modify spacemacs-theme
          (r|set-pair-faces
           ;; Themes to cycle in the following list
           (spacemacs-dark spacemacs-light) ;; doom-molokai

           ;; Variables
           (;; Palette from desktop color scheme
            (dark-1             "#2E3440")
            (dark-2             "#3B4252")
            (dark-3             "#434C5E")
            (dark-4             "#4C566A")
            (light-1            "#D8DEE9")
            (light-2            "#E5E9F0")
            (light-3            "#ECEFF4")
            (accent-dark        "#1C2028")
            (accent-dark-gray   (color-12-to-6 (color-darken-name accent-dark 1)))
            (accent-light       "#8a9899")
            (accent-shade-1     "#8FBCBB")
            (accent-shade-2     "#88C0D0")
            (accent-shade-3     "#81A1C1")
            (accent-shade-4     "#5E81AC")
            (colors-blue        accent-shade-4)
            (colors-blue-2      accent-shade-3)
            (colors-red         "#BF616A")
            (colors-orange      "#8FBCBB")
            (colors-yellow      "#8a9899")
            (colors-green       "#A3BE8C")
            (colors-purple      "#B48EAD")

            ;; For use in levelified faces set
            (level-1            colors-blue)
            (level-2            colors-blue-2)
            (level-3            colors-purple)
            (level-4            colors-orange)
            (level-5            accent-shade-3)
            (level-6            colors-green)
            (level-7            accent-shade-2)
            (level-8            colors-yellow)
            (level-9            accent-shade-1)

            ;; Base gray shades
            (bg-white           "#FEFFF9")
            (bg-dark            accent-dark-gray)
            (bg-darker          accent-dark)
            (bg-dark-solaire    (color-12-to-6 (color-lighten-name accent-dark 2)))
            (fg-white           light-3)
            (shade-white        (color-12-to-6 (color-lighten-name light-1 10)))
            (highlight          (color-12-to-6 (color-lighten-name accent-dark 4)))
            (region-dark        (color-12-to-6 (color-lighten-name accent-dark 50)))
            (region             dark-3)
            (slate              accent-shade-3)
            (gray               (color-12-to-6 (color-lighten-name dark-4 20)))

            ;; Programming
            (comment            (color-12-to-6 (color-lighten-name dark-4 2)))
            (doc                (color-12-to-6 (color-lighten-name dark-4 20)))
            (keyword            colors-blue)
            (builtin            colors-orange)
            (variable-name      colors-yellow)
            (function-name      accent-shade-2)
            (constant           colors-purple)
            (type               accent-shade-1)
            (string             colors-green)

            ;; Fonts
            (sans-font          "Source Sans Pro")
            (et-font            "EtBembo")
            (mono-font          "Iosevka"))

           ;; Settings
           (
            ;; NOTE: Basic fonts are set by other functions
	    ;;
            ;; (default
            ;;  (:background "unspecified-bg")
            ;;  (:background "unspecified-bg"))
            ;; (fixed-pitch
            ;;  (:family ,mono-font)
            ;;  (:family ,mono-font))
            ;; (variable-pitch
            ;;  ;; (:family ,sans-font
            ;;  ;;          :height 1.1)
            ;;  (:family ,et-font
            ;;           :background nil
            ;;           :foreground ,bg-dark
            ;;           :height 1.0))

	          ;; font-lock faces
            (font-lock-comment-face
             (:background "unspecified-bg" :slant italic)
             (:background "unspecified-bg" :slant italic))
            (font-lock-comment-delimiter-face
             (:background "unspecified-bg")
             (:background "unspecified-bg"))
            ;; something general
            (match
             (:underline ,colors-red)
             nil)
            (fringe
             (:background: "unspecified-bg")
             (:background: "unspecified-bg"))

            ;; tab-line
            ;; (tab-line
            ;;  (:background "dark violet" :foreground "white")
            ;;  nil)
            ;; (tab-line-highlight
            ;;  (:background "grey85" :foreground "black"
            ;;               :box (:line-width (1 . 1) :style released-button))
            ;;  nil)
            ;; (tab-line-tab
            ;;  (:inherit tab-line :box (:line-width (1 . 1) :style released-button))
            ;;  nil)
            ;; (tab-line-tab-current
            ;;  (:background "magenta" :foreground "#b2b2b2")
            ;;  nil)
            ;; (tab-line-tab-inactive
            ;;  (:background "purple" :foreground "gray")
            ;;  nil)
            ;; (tab-line-tab-modified
            ;;  (:foreground "yellow")
            ;;  nil)

	          ;; TODO: LanguageTool
            ;; (langtool-errline
            ;;  (:inherit flycheck-error)
            ;;  (:inherit flycheck-error))
            ;; (langtool-correction-face
            ;;  (:inherit flycheck-info)
            ;;  (:inherit flycheck-info))

	          ;; markdown
            (markdown-blockquote-face
             (:inherit org-quote)
             (:inherit org-quote))
            (markdown-bold-face
             (:inherit bold)
             (:inherit bold))
            (markdown-code-face
             (:inherit org-code)
             (:inherit org-code))
            (markdown-header-delimiter-face
             (:inherit org-level-1)
             (:inherit org-level-1))
            (markdown-header-face
             (:inherit org-level-1)
             (:inherit org-level-1))
            (markdown-header-face-1
             (:inherit org-level-1)
             (:inherit org-level-1))
            (markdown-header-face-2
             (:inherit org-level-2)
             (:inherit org-level-2))
            (markdown-header-face-3
             (:inherit org-level-3)
             (:inherit org-level-3))
            (markdown-header-face-4
             (:inherit org-level-4)
             (:inherit org-level-4))
            (markdown-header-face-5
             (:inherit org-level-5)
             (:inherit org-level-5))
            (markdown-header-face-6
             (:inherit org-level-6)
             (:inherit org-level-6))
            (markdown-inline-code-face
             (:inherit org-code)
             (:inherit org-code))
            (markdown-italic-face
             (:inherit italic)
             (:inherit italic))
            (markdown-link-face
             (:inherit org-link)
             (:inherit org-link))
            (markdown-list-face
             (:inherit org-list-dt)
             (:inherit org-list-dt))
            (markdown-metadata-key-face
             (:inherit font-lock-keyword-face)
             (:inherit font-lock-keyword-face))
            (markdown-pre-face
             (:inherit org-block)
             (:inherit org-block))
            (markdown-url-face
             (:inherit org-link)
             (:inherit org-link))

	          ;; org faces
            (org-agenda-date
             (:inherit variable-pitch :height 1.2)
             (:inherit nil))
            (org-agenda-date-today
             (:inherit variable-pitch :height 1.4)
             (:inherit nil))
            (org-agenda-date-weekend
             (:inherit variable-pitch :height 1.0)
             (:inherit nil))
            (org-agenda-done
             (:strike-through t)
             (:strike-through t))
            (org-agenda-structure
             (:inherit variable-pitch :height 1.3)
             (:inherit nil))
            (org-block
             (:inherit fixed-pitch :extend t :background "unspecified-bg")
             (:inherit fixed-pitch :extend t :background "unspecified-bg"))
            (org-block-begin-line
             (:inherit fixed-pitch-serif :background "unspecified-bg"
                       :overline t :underline nil :extend t)
             (:inherit fixed-pitch-serif :background "unspecified-bg"
                       :overline t :underline nil :extend t))
            (org-block-end-line
             (:inherit fixed-pitch-serif :background "unspecified-bg"
                       :overline nil :underline t :extend t)
             (:inherit fixed-pitch-serif :background "unspecified-bg"
                       :overline nil :underline t :extend t))
            (org-code
             (:inherit fixed-pitch)
             (:inherit fixed-pitch))
            (org-verbatim
             (:inherit fixed-pitch)
             (:inherit fixed-pitch))
            (org-column
             (:weight bold)
             nil)
            (org-column-title
             (:underline t)
             nil)
            ;; (org-date
            ;;  nil
            ;;  (:height 0.8))
            (org-document-info
             (:slant italic)
             (:slant italic :height 1.2 ))
            (org-document-info-keyword
             nil
             (:height 0.8))
            (org-document-title
             (:height 1.3 :underline t :weight bold)
             (:forground color-purple :height 1.4 :underline nil :weight bold))
            (org-done
             (:inherit variable-pitch)
             (:strike-through t :family ,et-font))
            (org-drawer
             (:height 0.8)
             (:height 0.8))
            (org-ellipsis
             (:underline nil)
             (:underline nil))
            (org-headline-done
             (:strike-through t)
             (:family ,et-font :strike-through t))
            (org-indent
             (:inherit org-hide)
             (:inherit org-hide))
            (org-level-1
             (:inherit variable-pitch :weight bold :height 1.2 )
             (:inherit nil :family ,et-font :height 1.3 :weight normal :slant normal))
            (org-level-2
             (:inherit variable-pitch :weight bold :height 1.15)
             (:inherit nil :family ,et-font :weight normal :slant italic :height 1.25))
            (org-level-3
             (:inherit variable-pitch :weight bold :height 1.1)
             (:inherit nil :family ,et-font :weight normal :slant italic :height 1.2))
            (org-level-4
             (:inherit variable-pitch :weight bold :height 1.05)
             (:inherit nil :family ,et-font :weight normal :slant italic :height 1.15))
            (org-level-5
             (:inherit variable-pitch :weight bold :height 1.0)
             (:inherit nil :family ,et-font :weight normal :slant italic :height 1.1))
            (org-level-6
             (:inherit variable-pitch :weight bold :height 1.0)
             (:inherit nil :family ,et-font :weight normal :slant italic :height 1.05))
            (org-level-7
             (:inherit variable-pitch :weight bold :height 1.0)
             (:inherit nil :family ,et-font :weight normal :slant italic :height 1.0))
            (org-level-8
             (:inherit variable-pitch :weight bold :height 1.0)
             (:inherit nil :family ,et-font :weight normal :slant italic :height 1.0))
            (org-link
             (:underline nil :weight normal)
             nil)
            (org-meta-line
             (:height 0.8)
             (:height 0.8))
            (org-quote
             (:slant italic)
             (:slant italic :family ,et-font))
            (org-ref-ref-face
             (:inherit org-link)
             (:inherit org-link))
            ;; (org-special-keyword
            ;;  (:height 0.8)
            ;;  (:height 0.8))
            (org-table
             (:height 0.8)
             (:height 0.8))
            ;; NOTE: Name is confusing, this is fixed pitch from org-variable-pitch package
            ;; (org-variable-pitch-face
            ;;  (:height 0.9)
            ;;  nil)
            ))
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
        (xy/mini-gui)
        ;; Reset fonts
        (xy/reset-fonts)
        (message "Adapt UI for graphical frame."))
    (progn

      ;; Some tweaks for terminal Emacs

      ;; Disable background colors in terminal frames
      ;;
      ;; REF: https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
      (set-face-background 'default "unspecified-bg" frame)
      (set-face-background 'font-lock-comment-face "unspecified-bg" frame)

      ;; `hl-line' package
      ;; (custom-set-faces
      ;;  '(hl-line ((t (:background "color-16" :extend t)))))

      ;; -- Try to unify UI-------------------------
      ;; ;; Add padding to emacs frame
      ;; ;; (spacious-padding-mode -1)
      ;; ;; (when (featurep 'spacious-padding) (spacious-padding-mode -1))
      ;; --------------------------------------------

      ;; default UI in text mode
      (xy/mini-gui)
      (message "Adapt UI config for terminal frame.")))
  (redraw-frame frame))
