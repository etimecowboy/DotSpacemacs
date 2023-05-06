;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- compleseus-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-05-06 Sat 04:04 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst compleseus-extra-packages
      '(
        ;;---- official compleseus layer packages
        embark
        consult
        marginalia
        ;; popwin ;; speck-checking layer
        ;;---- added packages
        consult-dir
        (eli-image :location local) ;; only works in minibuffer, not in vertico-posframe-mode
        ;; consult-project-extra ;; not as good as consult-projectile
        consult-projectile
        ;; consult-flycheck
        ;; consult-flyspell
        ;; consult-company ;; remove all company staff
        consult-org-roam
        yasnippet
        yasnippet-snippets
	      ;; vertico-quick
	      ;; vertico-repeat
	      vertico-posframe
        ))

(defun compleseus-extra/pre-init-embark ()
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
    (define-key embark-file-map
                (kbd "o") (xy/embark-ace-action find-file))
    (define-key embark-buffer-map
                (kbd "o") (xy/embark-ace-action switch-to-buffer))
    (define-key embark-bookmark-map
                (kbd "o") (xy/embark-ace-action bookmark-jump))
    (define-key embark-org-link-map
                (kbd "o") (xy/embark-ace-action org-open-at-point))

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
    (define-key embark-file-map
                (kbd "2") (xy/embark-split-action find-file split-window-below))
    (define-key embark-buffer-map
                (kbd "2") (xy/embark-split-action switch-to-buffer split-window-below))
    (define-key embark-bookmark-map
                (kbd "2") (xy/embark-split-action bookmark-jump split-window-below))
    (define-key embark-org-link-map
                (kbd "2") (xy/embark-split-action org-open-at-point split-window-below))
    (define-key embark-file-map
                (kbd "3") (xy/embark-split-action find-file split-window-right))
    (define-key embark-buffer-map
                (kbd "3") (xy/embark-split-action switch-to-buffer split-window-right))
    (define-key embark-bookmark-map
                (kbd "3") (xy/embark-split-action bookmark-jump split-window-right))
    (define-key embark-org-link-map
                (kbd "3") (xy/embark-split-action org-open-at-point split-window-right))
    (define-key embark-file-map
                (kbd "S") 'sudo-find-file)
    (define-key embark-bookmark-map
                (kbd "S") 'sudo-find-file)
    ))

(defun compleseus-extra/pre-init-consult ()
  (spacemacs|use-package-add-hook consult
    :post-config
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

    ;; (require 'consult-xref)
    ;; consult-xref
    consult--source-bookmark
    consult--source-file-register
    consult--source-recent-file
    consult--source-project-recent-file

    ;; REF: https://github.com/minad/consult/blob/main/README.org#miscellaneous
    ;; ;; Use `consult-completion-in-region' if Vertico is enabled.
    ;; ;; Otherwise use the default `completion--in-region' function.
    ;; (setq completion-in-region-function
    ;;       (lambda (&rest args)
    ;;         (apply (if vertico-mode
    ;;                    #'consult-completion-in-region
    ;;                  #'completion--in-region)
    ;;                args)))

    ;; REF: https://github.com/minad/consult/issues/350
    ;; vertico-mode is enabled at startup and I might then disable
    ;; it interactively to quickly try something else.
    (setq completion-in-region-function
          (lambda (start end collection &optional predicate)
            (if vertico-mode
                (consult-completion-in-region start end collection predicate)
              (completion--in-region start end collection predicate))))
    ))

(defun compleseus-extra/pre-init-marginalia ()
  (spacemacs|use-package-add-hook marginalia
    :post-config
    (setq marginalia-separator "  |  ")
    ))

(defun compleseus-extra/init-consult-dir ()
  (use-package consult-dir
    :ensure t
    :bind (("C-x C-d" . consult-dir)
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file))))

(defun compleseus-extra/init-eli-image ()
  (use-package eli-image
    :commands (eli-select-images)
    :bind (("M-I" . xy/eli-select-images))
    ))

;; (defun compleseus-extra/init-consult-project-extra ()
;;   (use-package consult-project-extra))

(defun compleseus-extra/init-consult-projectile ()
  (use-package consult-projectile
    :bind (
           ("M-s p" . consult-projectile)
           )))

;; remove all company staff
;; (defun compleseus-extra/init-consult-company ()
;;   (use-package consult-company
;;     :bind (
;;            ("M-s c" . consult-company)
;;            )
;;     ))

;; load consult-org-roam
(defun compleseus-extra/init-consult-org-roam ()
  (spacemacs|use-package-add-hook org-roam
    :post-config (require 'consult-org-roam))
  (use-package consult-org-roam
    :after org-roam
    :custom
    ;; Use `ripgrep' for searching with `consult-org-roam-search'
    (consult-org-roam-grep-func #'consult-ripgrep)
    ;; Configure a custom narrow key for `consult-buffer'
    (consult-org-roam-buffer-narrow-key ?r)
    ;; Display org-roam buffers right after non-org-roam buffers
    ;; in consult-buffer (and not down at the bottom)
    (consult-org-roam-buffer-after-buffers t)
    :config
    (consult-org-roam-mode 1)
    ;; Eventually suppress previewing for certain functions
    (consult-customize
     consult-org-roam-forward-links
     :preview-key (kbd "M-."))
    (spacemacs|diminish consult-org-roam-mode)
    :bind
    ("M-s n" . consult-org-roam-file-find)
    ("M-s b" . consult-org-roam-backlinks)
    ("M-s s" . consult-org-roam-search)
    ("M-s F" . consult-org-roam-forward-links)
    ("M-s a" . consult-org-agenda)
    ("M-s h" . consult-org-heading)
    ("M-s I" . consult-info)
    ("M-s M" . consult-man)
    ("M-s y" . consult-yasnippet)
    ("M-y"   . consult-yank-replace)
    ("M-s i" . org-roam-node-insert)
    ))

(defun compleseus-extra/init-yasnippet ()
  (use-package yasnippet
    :ensure t
    :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
    :init
    (defvar yas-snippet-dirs nil)
    (setq auto-completion-private-snippets-directory "/home/xin/src/spacemacs/private/snippets")
    (add-to-list 'yas-snippet-dirs 'auto-completion-private-snippets-directory)
    :config
    (yas-global-mode 1)
    (spacemacs|diminish yas-minor-mode " ⓨ" " y")
    ))

(defun compleseus-extra/init-yasnippet-snippets ())

;; (defun compleseus-extra/init-vertico-quick ()
;;   (use-package vertico-quick))

;; (defun compleseus-extra/init-vertico-repeat ()
;;   (use-package vertico-repeat))

(defun compleseus-extra/init-vertico-posframe ()
  (use-package vertico-posframe
    :ensure t
    :after (vertico posframe)
    :config
    (setq vertico-posframe-fallback-mode 'vertico-buffer-mode
          vertico-posframe-poshandler 'posframe-poshandler-point-frame-center)
    (vertico-posframe-mode t)
    ))
