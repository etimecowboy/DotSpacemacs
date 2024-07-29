;;; config.el --- ui configuration File for Spacemacs
;; Time-stamp: <2024-07-16 Tue 00:47:07 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>

;; Default text font in Emacs
(defvar xy:default-font "Sarasa Fixed SC Nerd Font" "The default font.")
(defvar xy:default-size 120 "The default font size.")

;; Fixed-pitch font
(defvar xy:fixed-pitch-font "Sarasa Fixed SC Nerd Font"
  "The font to use for monospaced text. `fixed-pitch' face.")
;; "Sarasa Fixed SC Nerd Font"
;; "Iosevka Fixed"
;; "Iosevka Fixed SS15"
(defvar xy:fixed-pitch-size 120
  "The font size to use for monospaced text.")
(defvar xy:fixed-pitch-serif-font "Iosevka Fixed Slab"
  "The serif font to use for serif monospaced text. `fixed-pitch-serif' face.")
;; "Noto Serif"
;; "Noto Serif CJK SC"
;; "Iosevka Fixed Slab"
(defvar xy:fixed-pitch-serif-size 120
  "The font size to use for serif monospaced text.")

;; Variable-pitch font
(defvar xy:variable-pitch-font "Noto Sans"
  "The font to use for proportional text. `variable-pitch' face.")
;; "Noto Sans" ;; NOTE: Don't use "Noto Sans CJK SC", the ’ ("It’s") of which will be double-width
;; "TeX Gyre Pagella" ;; Typoral's font
;; "Linux Libertine O" ;; Wikipedia's font
;; "Liberation Sans"
;; "Liberation Sans Narrow"
;; "EtBembo" ;; etbook's font, used for demo
;; "Iosevka Aille" ;; pseudo-/semi-propotional font
;; "FreeSans" ;; Not smooth display on my LCD, requires more spaces between lines
;; "Roboto"
(defvar xy:variable-pitch-size 120
  "The font size to use for proportional text.")
(defvar xy:variable-pitch-text-size 1.2
  "The height scale for `variable-pitch-text' face.")

;; CJK font
(defvar xy:cjk-font "Sarasa Fixed SC Nerd Font"
  "The font to use for CJK characters.")
;; "Sarasa Fixed SC Nerd Font" ;; NOTE: it supports italic style
;; "Adobe KaiTi Std"
;; "LXGW WenKai GB"
;; "FangSong"
;; "Noto Serif CJK SC"
(defvar xy:cjk-size 1.2 "The font size to use for CJK characters.")

;; :variables
;; xy:fixed-pitch-font "Sarasa Fixed SC Nerd Font"
;; ;; "Iosevka Fixed Curly"
;; xy:variable-pitch-font "Linux Biolinum O"
;; ;; "Linux Libertine O"
;; ;; "Iosevka Aile"
;; xy:fixed-pitch-serif-font "Linux Libertine O"
;; ;; "Linux Biolinum O"
;; ;; "Iosevka Fixed Curly Slab"
;; xy:variable-pitch-text-height 1.2
;; xy:display-mixed-pitch nil)

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

;; modify themes
(r|set-pair-faces
 ;; Themes to cycle in the following list
 (spacemacs-dark spacemacs-light) ;; doom-molokai

 ;; Variables
 (;; Palette from desktop color scheme
  (dark-1 "#2E3440") (dark-2 "#3B4252") (dark-3 "#434C5E") (dark-4 "#4C566A")
  (light-1 "#D8DEE9") (light-2 "#E5E9F0") (light-3 "#ECEFF4") (accent-dark "#1C2028")
  (accent-dark-gray (color-12-to-6 (color-darken-name accent-dark 1)))
  (accent-light "#8a9899") (accent-shade-1 "#8FBCBB") (accent-shade-2 "#88C0D0")
  (accent-shade-3 "#81A1C1") (accent-shade-4 "#5E81AC") (colors-blue accent-shade-4)
  (colors-blue-2 accent-shade-3) (colors-red "#BF616A") (colors-orange "#8FBCBB")
  (colors-yellow "#8a9899") (colors-green "#A3BE8C") (colors-purple "#B48EAD")

  ;; For use in levelified faces set
  (level-1 colors-blue) (level-2 colors-blue-2) (level-3 colors-purple)
  (level-4 colors-orange) (level-5 accent-shade-3) (level-6 colors-green)
  (level-7 accent-shade-2) (level-8 colors-yellow) (level-9 accent-shade-1)

  ;; Base gray shades
  (bg-white "#FEFFF9") (bg-dark accent-dark-gray) (bg-darker accent-dark)
  (bg-dark-solaire (color-12-to-6 (color-lighten-name accent-dark 2)))
  (fg-white light-3)
  (shade-white (color-12-to-6 (color-lighten-name light-1 10)))
  (highlight (color-12-to-6 (color-lighten-name accent-dark 4)))
  (region-dark (color-12-to-6 (color-lighten-name accent-dark 50)))
  (region dark-3) (slate accent-shade-3)
  (gray (color-12-to-6 (color-lighten-name dark-4 20)))

  ;; Programming
  (comment (color-12-to-6 (color-lighten-name dark-4 2)))
  (doc (color-12-to-6 (color-lighten-name dark-4 20)))
  (keyword colors-blue) (builtin colors-orange)
  (variable-name colors-yellow) (function-name accent-shade-2)
  (constant colors-purple) (type accent-shade-1) (string colors-green)

  ;; Fonts
  (sans-font "Source Sans Pro")
  (et-font "EtBembo")
  (mono-font "Iosevka"))

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
  ;; -----------------------------
  (font-lock-comment-face
   nil
   (:slant italic)
   ;; (:background nil :slant italic)
   ;; (:background nil :slant italic)
   )
  ;; (font-lock-comment-delimiter-face
  ;;  (:background "unspecified-bg")
  ;;  (:background "unspecified-bg")
  ;;  ;; (:background nil)
  ;;  ;; (:background nil)
  ;;  )
  ;; ------------------------------

  ;; something general
  (match
   (:underline ,colors-red)
   nil)

  ;; --------------------------------
  (fringe
   ;; (:background: nil)
   ;; (:background: nil)
   (:background: "unspecified-bg")
   (:background: "unspecified-bg")
   )
  ;; ---------------------------------

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
  ;; ---------------------------------------------
  (org-block
   (:inherit fixed-pitch :extend t
             :background "unspecified-bg"
             ;; :background nil
             )
   (:inherit fixed-pitch :extend t
             ;; :background nil
             :background "unspecified-bg"
             ))
  (org-block-begin-line
   (:inherit fixed-pitch-serif
             ;; :background nil
             :background "unspecified-bg"
             :overline t :underline nil :extend t)
   (:inherit fixed-pitch-serif
             ;; :background nil
             :background "unspecified-bg"
             :overline t :underline nil :extend t))
  (org-block-end-line
   (:inherit fixed-pitch-serif
             ;; :background nil
             :background "unspecified-bg"
             :overline nil :underline t :extend t)
   (:inherit fixed-pitch-serif
             ;; :background nil
             :background "unspecified-bg"
             :overline nil :underline t :extend t))
  ;; -----------------------------------------------------
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
