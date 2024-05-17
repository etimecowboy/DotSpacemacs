;;; config.el --- ui configuration File for Spacemacs
;; Time-stamp: <2024-04-29 Mon 01:59:15 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>

(defvar xy:default-font "Sarasa Fixed SC Nerd Font" "The default font.")
(defvar xy:default-size 120 "The default font size.")

(defvar xy:fixed-pitch-font "Sarasa Fixed SC Nerd Font"
  "The font to use for monospaced text. `fixed-pitch' face.")
;; "Sarasa Fixed SC Nerd Font"
;; "Noto Sans Mono"
;; "Noto Sans Mono CJK SC"
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

(defvar xy:variable-pitch-font "Noto Sans"
  "The font to use for proportional text. `variable-pitch' face.")
;; "Liberation Sans"
;; "Liberation Sans Narrow"
;; "TeX Gyre Pagella" ;; Typoral's font
;; "Linux Libertine O" ;; Wikipedia's font
;; "Noto Sans CJK SC"
;; "Iosevka Aille"
;; "EtBembo" ;; etbook's font
(defvar xy:variable-pitch-size 120
  "The font size to use for proportional text.")
(defvar xy:variable-pitch-text-size 1.2
  "The height scale for `variable-pitch-text' face.")

(defvar xy:cjk-font "Sarasa Fixed SC Nerd Font"
  "The font to use for CJK characters.")
;; "Sarasa Fixed SC Nerd Font" ;; NOTE: it supports italic style
;; "Adobe KaiTi Std"
;; "LXGW WenKai GB"
;; "FangSong"
;; "Noto Serif CJK SC"
(defvar xy:cjk-size 1.2
  "The font size to use for CJK characters.")

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
