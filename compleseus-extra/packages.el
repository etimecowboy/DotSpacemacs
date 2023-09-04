;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- compleseus-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-09-04 Mon 02:04 by xin on tufg>
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
    orderless
    ;;---- packages that belongs to other layers
    hippie-exp ;; auto-complete layer
    ;;---- added packages
    consult-dir
    (eli-image :location local)
    consult-projectile
    consult-org-roam
    vertico-posframe
    (org-preview-image-link-posframe :location local)
    hyperbole
    cape
    ;; corfu
    ;; corfu-terminal
    ;; corfu-doc
    ;; kind-icon
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


(defun compleseus-extra/post-init-embark ()
  (spacemacs|use-package-add-hook embark
    :post-config
    (setq embark-quit-after-action t)
    ;; "embark-consult.el" commentary
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
    (add-to-list 'display-buffer-alist
                 '("\\*Embark" display-buffer-same-window)
                 ;; '("Embark\\ Live" display-buffer-pop-up-frame)
                 ;; '("Embark\\ Export" display-buffer-at-bottom)
                 ;; '("^\\*Embark\\ Live.*\\*$" display-buffer-pop-up-frame)
                 ;; '("^\\*Embark\\ Export.*\\*$" display-buffer-at-bottom)
                 ;; '("^\\*Embark.*\\*$" display-buffer-at-bottom)
                 )
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
    ))


(defun compleseus-extra/post-init-consult ()
  (consult-customize consult-theme
                     :preview-key '("M-." "C-SPC" :debounce 0.2 any)
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
                     :preview-key '("M-." "C-SPC" :debounce 0.75 any))

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
  )


(defun compleseus-extra/post-init-marginalia ()
  (setq marginalia-separator "  |  "))


(defun compleseus-extra/post-init-orderless ())


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


(defun compleseus-extra/init-consult-projectile ()
  (use-package consult-projectile
    :bind (
           ("M-s p" . consult-projectile)
           )))


;; load consult-org-roam
(defun compleseus-extra/init-consult-org-roam ()
  (spacemacs|use-package-add-hook org-roam
    :post-config (require 'consult-org-roam))
  (use-package consult-org-roam
    :after org-roam
    :init
    ;; (advice-add 'consult-org-roam-file-find :before 'org-roam-db-sync)
    (advice-add 'consult-org-roam-backlinks :before 'org-roam-db-sync)
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
    ("M-s b" . consult-org-roam-backlinks)
    ("M-s s" . consult-org-roam-search)
    ("M-s F" . consult-org-roam-forward-links)
    ("M-s a" . consult-org-agenda)
    ("M-s h" . consult-org-heading)
    ("M-s I" . consult-info)
    ("M-s M" . consult-man)
    ("M-s y" . consult-yasnippet)
    ("M-y"   . consult-yank-replace)
    ("M-s n" . org-roam-node-find) ;; replace default consult-org-roam-file-find
    ("M-s i" . org-roam-node-insert)
    ("M-s R" . org-roam-ref-find)
    ))

(defun compleseus-extra/init-vertico-posframe ()
  (use-package vertico-posframe
    :commands vertico-posframe-mode
    :defer t
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
    :defer t
    :config
    (spacemacs|diminish hyperbole-mode)
    ;; (spacemacs|diminish hyperbole-mode " Ⓗ" " H")
    ;; (setq hyperbole-mode-lighter " Ⓗ")
    ;; :bind*
    ;; ("C-:" . hkey-either)
    ;; :bind
    ;; ("M-o" . nil) ;;conflict with embark
    ))


(defun compleseus-extra/init-hippie-exp ()
  (use-package hippie-expand
    :bind ([repmap dabbrev-expand] . hippie-expand)
    :commands hippie-expand
    :config
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-expand-line))
    ))


(defun compleseus-extra/init-cape ()
  (use-package cape
    :init
    ;; Add to the global default value of `completion-at-point-functions' which is
    ;; used by `completion-at-point'.  The order of the functions matters, the
    ;; first function returning a result wins.  Note that the list of buffer-local
    ;; completion functions takes precedence over the global list.
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    ;;(add-to-list 'completion-at-point-functions #'cape-history)
    ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
    ;;(add-to-list 'completion-at-point-functions #'cape-tex)
    ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
    ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;;(add-to-list 'completion-at-point-functions #'cape-dict)
    ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
    ;;(add-to-list 'completion-at-point-functions #'cape-line)

    (spacemacs|define-transient-state cape
      :title "completion-at-point transient state (with cape)"
      :doc "
^Word^                  ^Character^                   ^Misc^
^^^^^^^^-----------------------------------------------------------------
[_d_] dabbrev           [_t_] TeX-format unicode      [_\\_] completion-at-point
[_a_] abbrev            [_&_] sgml-format unicode     [_f_] file path
[_w_] English word      [_r_] rfc1345-format unicode  [_l_] line of text
[_s_] Elisp symbol                                  [_h_] eshell history
[_e_] Elisp in Org or Markdown code block           [_q_] quit
[_k_] programming language keyword
"
      :bindings
      ("q"  nil :exit t)
      ("\\" completion-at-point :exit t)
      ("g"  complete-tag :exit t)
      ("d"  cape-dabbrev :exit t)
      ("h"  cape-history :exit t)
      ("f"  cape-file :exit t)
      ("k"  cape-keyword :exit t)
      ("s"  cape-symbol :exit t)
      ("e"  cape-elisp-block :exit t)
      ("a"  cape-abbrev :exit t)
      ("l"  cape-line :exit t)
      ("w"  cape-dict :exit t)
      ("t"  cape-tex :exit t)
      ("&"  cape-sgml :exit t)
      ("r"  cape-rfc1345 :exit t))
    ))


;; (defun compleseus-extra/init-corfu ()
;;   (use-package corfu
;;     :defer t
;;     ;; Optional customizations
;;     :custom
;;     (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;     (corfu-auto t)                 ;; Enable auto completion
;;     (corfu-separator ?\s)          ;; Orderless field separator
;;     (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;     (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;     (corfu-preview-current nil)    ;; Disable current candidate preview
;;     (corfu-preselect 'prompt)      ;; Preselect the prompt
;;     (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;     (corfu-scroll-margin 5)        ;; Use scroll margin

;;     ;; Enable Corfu only for certain modes.
;;     :hook (;; (prog-mode . corfu-mode)
;;            ;; (shell-mode . corfu-mode)
;;            ;; (eshell-mode . corfu-mode)
;;            (org-mode . corfu-mode)
;;            )

;;     ;; Recommended: Enable Corfu globally.
;;     ;; This is recommended since Dabbrev can be used globally (M-/).
;;     ;; See also `global-corfu-modes'.
;;     ;; :init
;;     ;; (global-corfu-mode)
;;     ))


;; (defun compleseus-extra/init-corfu-terminal ()
;;   (use-package corfu-terminal
;;     :defer t))


;; (defun compleseus-extra/init-corfu-doc ()
;;   (use-package corfu-doc
;;     :defer t
;;     ;; :after corfu
;;     ;; :hook (corfu-mode . corfu-doc-mode)
;;     :custom
;;     (corfu-doc-delay 0.5)
;;     (corfu-doc-max-width 70)
;;     (corfu-doc-max-height 20)

;;     ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
;;     ;; extra-safe that this is set when corfu-doc is loaded. I do not want
;;     ;; documentation shown in both the echo area and in the `corfu-doc' popup.
;;     (corfu-echo-documentation nil)
;;     ))


;; (defun compleseus-extra/init-kind-icon ()
;;   (use-package kind-icon
;;     :after corfu
;;     :custom
;;     (kind-icon-default-face 'corfu-default)
;;     :config
;;     (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
;;     ))
