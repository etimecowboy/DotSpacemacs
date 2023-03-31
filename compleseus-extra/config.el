;;; config.el --- Compleseus-extra configuration File for Spacemacs
;; Time-stamp: <2023-03-29 Wed 10:25 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

(spacemacs|use-package-add-hook embark
  :post-config
  (add-to-list 'display-buffer-alist
               '("\\*Embark" display-buffer-same-window)
               ;; '("Embark\\ Live" display-buffer-pop-up-frame)
               ;; '("Embark\\ Export" display-buffer-at-bottom)
               ;; '("^\\*Embark\\ Live.*\\*$" display-buffer-pop-up-frame)
               ;; '("^\\*Embark\\ Export.*\\*$" display-buffer-at-bottom)
               ;; '("^\\*Embark.*\\*$" display-buffer-at-bottom)
               )
  (setq embark-quit-after-action t)

  ;;REF: https://karthinks.com/software/fifteen-ways-to-use-embark/
  (eval-when-compile
    (defmacro xy/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let* ((aw-dispatch-always t)
                  (embark-quit-after-action t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map     (kbd "o") (xy/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (xy/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (xy/embark-ace-action bookmark-jump))
  (define-key embark-org-link-map (kbd "o") (xy/embark-ace-action org-open-at-point))

  (eval-when-compile
    (defmacro xy/embark-split-action (fn split-type)
      `(defun ,(intern (concat "xy/embark-"
                               (symbol-name fn)
                               "-"
                               (car (last (split-string
                                           (symbol-name split-type) "-"))))) ()
         (interactive)
         (funcall #',split-type)
         (call-interactively #',fn))))

  (define-key embark-file-map     (kbd "2") (xy/embark-split-action find-file split-window-below))
  (define-key embark-buffer-map   (kbd "2") (xy/embark-split-action switch-to-buffer split-window-below))
  (define-key embark-bookmark-map (kbd "2") (xy/embark-split-action bookmark-jump split-window-below))
  (define-key embark-org-link-map (kbd "2") (xy/embark-split-action org-open-at-point split-window-below))
  (define-key embark-file-map     (kbd "3") (xy/embark-split-action find-file split-window-right))
  (define-key embark-buffer-map   (kbd "3") (xy/embark-split-action switch-to-buffer split-window-right))
  (define-key embark-bookmark-map (kbd "3") (xy/embark-split-action bookmark-jump split-window-right))
  (define-key embark-org-link-map (kbd "3") (xy/embark-split-action org-open-at-point split-window-right))


  (define-key embark-file-map (kbd "S") 'sudo-find-file)
  (define-key embark-bookmark-map (kbd "S") 'sudo-find-file)
  )

;; (with-eval-after-load "embark"
;;   (embark-define-keymap embark-org-heading-map
;;                         "Keymap for Embark heading actions."
;;                         ("i" embark-insert)
;;                         ("b" consult-heading-insert-backlink)
;;                         ("w" embark-copy-as-kill)
;;                         ("q" embark-toggle-quit)
;;                         ("E" embark-export)
;;                         ("S" embark-collect)
;;                         ("L" embark-live)
;;                         ("B" embark-become)
;;                         ("A" embark-act-all)
;;                         ("C-s" embark-isearch)
;;                         ("SPC" mark)
;;                         ("DEL" delete-region))

;;   (embark-define-keymap embark-org-roam-map
;;                         "Keymap for Embark org roam actions."
;;                         ("i" org-roam-node-insert)
;;                         ("s" embark-collect)
;;                         ("b" eli-org-roam-backlinks-node-read))

;;   (add-to-list 'embark-keymap-alist
;;                '(consult-org-heading . embark-org-heading-map))
;;   (add-to-list 'embark-keymap-alist
;;                '(org-roam-node . embark-org-roam-map)))

;; REF:
;;   - https://github.com/doomemacs/doomemacs/issues/7064
;;   - https://github.com/syl20bnr/spacemacs/pull/15928
;;   - https://github.com/minad/consult#use-package-example

(spacemacs|use-package-add-hook consult
  :post-config
  ;; (require 'consult-xref)
  (consult-customize
   consult-theme
   :preview-key '("M-.")
   consult-buffer
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-yank-pop
   :preview-key '("M-."))
   ;; consult-xref
   consult--source-bookmark
   consult--source-file-register
   consult--source-recent-file
   consult--source-project-recent-file
  )

(spacemacs|use-package-add-hook marginalia
  :post-config
  (setq marginalia-separator "  |  "))
