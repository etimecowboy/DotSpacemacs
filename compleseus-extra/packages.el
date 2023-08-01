;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- compleseus-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-08-01 Tue 09:28 by xin on tufg>
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
    ;;---- added packages
    consult-dir
    (eli-image :location local)
    consult-projectile
    consult-org-roam
    yasnippet
    yasnippet-snippets
    vertico-posframe
    (org-preview-image-link-posframe :location local)
    hyperbole
    ace-link
    ;; (hyperbole :location
    ;;            (recipe
    ;;             :fetcher git
    ;;             :url "https://git.savannah.gnu.org/git/hyperbole.git"
    ;;             :files ("*")
    ;;             ))
    ;; capf-autosuggest
    ;; consult-project-extra ;; not as good as consult-projectile
    ;; consult-flycheck
    ;; consult-flyspell
    ;; consult-company ;; remove all company staff
	  ;; vertico-quick
	  ;; vertico-repeat
    ;; popwin ;; speck-checking layer
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
      (defmacro xy|embark-ace-action (fn)
        `(defun ,(intern (concat "xy/embark-ace-" (symbol-name fn))) ()
           (interactive)
           (with-demoted-errors "%s"
             (require 'ace-window)
             (let* ((aw-dispatch-always t)
                    ;; Add this line if the user set `embark-quit-after-action' to nil
                    (embark-quit-after-action t)
                    ;; Add this line to fix Fix xy/embark-ace-org-open-at-point
                    (cur (buffer-name))
                    )
               (aw-switch-to-window (aw-select nil))
               ;; Add this line to fix Fix xy/embark-ace-org-open-at-point
               (switch-to-buffer cur)
               (call-interactively (symbol-function ',fn)))))))
    (define-key embark-file-map
                (kbd "o")
                (xy|embark-ace-action find-file))
    (define-key embark-buffer-map
                (kbd "o")
                (xy|embark-ace-action switch-to-buffer))
    (define-key embark-bookmark-map
                (kbd "o")
                (xy|embark-ace-action bookmark-jump))
    (define-key embark-org-link-map
                (kbd "o")
                (xy|embark-ace-action org-open-at-point))
    (define-key org-mode-map
                (kbd "C-c C-S-o")
                (xy|embark-ace-action org-open-at-point))

    (eval-when-compile
      (defmacro xy|embark-split-action (fn split-type)
        `(defun ,(intern (concat "xy/embark-"
                                 (symbol-name fn)
                                 "-"
                                 (car (last (split-string
                                             (symbol-name split-type) "-"))))) ()
           (interactive)
           (funcall #',split-type)
           (call-interactively #',fn))))
    (define-key embark-file-map
                (kbd "2")
                (xy|embark-split-action find-file split-window-below))
    (define-key embark-buffer-map
                (kbd "2")
                (xy|embark-split-action switch-to-buffer split-window-below))
    (define-key embark-bookmark-map
                (kbd "2")
                (xy|embark-split-action bookmark-jump split-window-below))
    (define-key embark-org-link-map
                (kbd "2")
                (xy|embark-split-action org-open-at-point split-window-below))
    (define-key embark-file-map
                (kbd "3")
                (xy|embark-split-action find-file split-window-right))
    (define-key embark-buffer-map
                (kbd "3")
                (xy|embark-split-action switch-to-buffer split-window-right))
    (define-key embark-bookmark-map
                (kbd "3")
                (xy|embark-split-action bookmark-jump split-window-right))
    (define-key embark-org-link-map
                (kbd "3")
                (xy|embark-split-action org-open-at-point split-window-right))
    (define-key embark-file-map
                (kbd "S") 'sudo-find-file)
    (define-key embark-bookmark-map
                (kbd "S") 'sudo-find-file)
    (define-key org-mode-map
                (kbd "C-c 3")
                (xy|embark-split-action org-open-at-point split-window-right))
    (define-key org-mode-map
                (kbd "C-c 2")
                (xy|embark-split-action org-open-at-point split-window-below))

    ;; "embark-consult.el" commentary
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
    ))

(defun compleseus-extra/pre-init-consult ()
  (spacemacs|use-package-add-hook consult
    :post-config
    (consult-customize
     consult-theme
     :preview-key '("M-." "C-SPC"
                    :debounce 0.2 any)
     consult-buffer
     consult-find
     consult-recent-file
     consult-locate
     consult-projectile
     consult-ripgrep
     consult-git-grep
     consult-grep
     consult-imenu
     consult-imenu-multi
     consult-bookmark
     consult-yank-pop
     consult-yasnippet
     consult-org-agenda
     :preview-key '("M-." "C-SPC"
                    :debounce 0.75 any))

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

    ;; FIXME: vertico--exhibit error
    ;; REF: https://github.com/minad/vertico/blob/main/README.org#debugging-vertico
    ;; (setq debug-on-error t)
    (defun force-debug (func &rest args)
      (condition-case e
          (apply func args)
        ((debug error) (signal (car e) (cdr e)))))
    (advice-add #'vertico--exhibit :around #'force-debug)

    ;; begin searching after 2 characters.
    (setq consult-async-min-input 2)
    ))

(defun compleseus-extra/pre-init-marginalia ()
  (spacemacs|use-package-add-hook marginalia
    :post-config
    (setq marginalia-separator "  |  ")
    ))

(defun compleseus-extra/init-consult-dir ()
  (use-package consult-dir
    :commands (consult-dir consult-dir-jump-file)
    :bind (("C-x C-d" . consult-dir)
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file))))

(defun compleseus-extra/init-eli-image ()
  (use-package eli-image
    :commands
    (eli-image-find-file eli-image-insert-path eli-image-org-attach)
    :bind (("C-x C-P" . eli-image-find-file)
           ("C-x C-p" . eli-image-insert-path)
           :map vertico-map
           ("C-x C-P" . eli-image-find-file)
           ("C-x C-p" . eli-image-insert-path))
    :config
    (setq eli-image-default-directory "~/下载/")
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
     consult-org-roam-search
     consult-org-roam-file-find
     consult-org-roam-forward-links
     consult-org-roam-backlinks
     :preview-key '("M-." "C-SPC"
                    :debounce 0.75 any))
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
    ("M-s R" . org-roam-ref-find)
    ))

(defun compleseus-extra/init-yasnippet ()
  (use-package yasnippet
    ;; :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
    :ensure t
    :init
    (defvar yas-snippet-dirs nil)
    (setq auto-completion-private-snippets-directory "/home/xin/src/spacemacs/private/snippets")
    (add-to-list 'yas-snippet-dirs 'auto-completion-private-snippets-directory)
    :config
    (spacemacs|diminish yas-minor-mode " ⓨ" " y")
    (yas-global-mode t)
    ))

(defun compleseus-extra/init-yasnippet-snippets ())

;; (defun compleseus-extra/init-capf-autosuggest ()
;;     (use-package capf-autosuggest
;;       :ensure t
;;       :hook (org-mode . capf-autosuggest-mode)
;;       ))

;; (defun compleseus-extra/init-vertico-quick ()
;;   (use-package vertico-quick))

;; (defun compleseus-extra/init-vertico-repeat ()
;;   (use-package vertico-repeat))

(defun compleseus-extra/init-vertico-posframe ()
  (use-package vertico-posframe
    :commands vertico-posframe-mode
    :after (vertico posframe)
    :config
    (setq vertico-posframe-fallback-mode 'vertico-buffer-mode
          vertico-posframe-poshandler 'posframe-poshandler-point-frame-center)
    ;; (vertico-posframe-mode t)
    ;; NOTE: In GUI mode, the posframes would be covered
    ;; by eaf windows, and become invisible.
    ))

(defun compleseus-extra/init-org-preview-image-link-posframe ()
  (use-package org-preview-image-link-posframe
    :commands org-preview-image-link-posframe
    :defer t
    ))

(defun compleseus-extra/init-hyperbole ()
  (use-package hyperbole
    :config
    (spacemacs|diminish hyperbole-mode " Ⓗ" " H")
    (setq hyperbole-mode-lighter " Ⓗ")
    ;; :bind*
    ;; ("C-:" . hkey-either)
    ;; :bind
    ;; ("M-o" . nil) ;;conflict with embark
    ))

(defun compleseus-extra/pre-init-ace-link ()
  (spacemacs|use-package-add-hook ace-link
    :pre-init
    ;; j or o to open links in ace style
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "jj" 'ace-link-org
      "jo" 'ace-link-org)
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
      "j" 'ace-link-org-agenda
      "o" 'ace-link-org-agenda)
    (spacemacs/set-leader-keys-for-major-mode 'Info-mode
      "j" 'ace-link-info)
    (spacemacs/set-leader-keys-for-major-mode 'Custom-mode
      "j" 'ace-link-custom
      "o" 'ace-link-custom)
    (spacemacs/set-leader-keys-for-major-mode 'compilation-mode
      "j" 'ace-link-compilation
      "o" 'ace-link-compilation)
    (spacemacs/set-leader-keys-for-major-mode 'xref--xref-buffer-mode
      "j" 'ace-link-xref
      "o" 'ace-link-xref)

    ;; not working
    ;; (spacemacs/set-leader-keys-for-major-mode 'Man-mode
    ;;   "j" 'ace-link-man)
    ;; (spacemacs/set-leader-keys-for-major-mode 'help-mode
    ;;   "j" 'ace-link-help)
    ))
