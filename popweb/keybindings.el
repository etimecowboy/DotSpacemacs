;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- popweb Layer keybindings File for Spacemacs
;; Time-stamp: <2023-03-13 Mon 07:20 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; (spacemacs/declare-prefix "od" "dictionary") ;; defined by chinese-extra layer
(spacemacs/declare-prefix "op" "popweb")
(spacemacs/declare-prefix "oc" "Chinese")

(spacemacs/set-leader-keys
     ;; "opc" 'popweb-color-picker-show
     "opp"  'popweb-url-preview-pointer
     "opb"  'popweb-dict-bing-pointer
     "opy"  'popweb-dict-youdao-pointer
     "opg"  'popweb-dict-youglish-pointer
     "opd"  'popweb-dict-dictcn-pointer
     "ops"  'popweb-dict-say-word
     "opL"  'popweb-org-roam-link-preview-select
     "opl"  'popweb-org-roam-link-show
     "opn"  'popweb-org-roam-node-preview-select
     "opr"  'popweb-org-roam-node-backlinks-preview
     "ocb"  'popweb-dict-bing-pointer
     "oco"  'popweb-dict-youdao-pointer
     "oy"   'popweb-dict-youdao-pointer
     "ocg"  'popweb-dict-youglish-pointer
     "oci"  'popweb-dict-dictcn-pointer
     "ocs"  'popweb-dict-say-word
     "aorS" 'popweb-org-roam-link-preview-select
     "aors" 'popweb-org-roam-link-show
     "aorn" 'popweb-org-roam-node-preview-select
     "aorp" 'popweb-org-roam-node-backlinks-preview
     )
