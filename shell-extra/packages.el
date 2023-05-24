;;; packages.el --- shell packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst shell-extra-packages
  '(vterm
    ;; multi-vterm ;; included in shell layer
    eshell
    ;; eshell-git-prompt
    ;; eshell-syntax-highlighting
    ;; capf-autosuggest
    ;; edh-autosuggest
    ;; eshell-up
    (aweshell :location (recipe :fetcher github :repo "manateelazycat/aweshell"))
    ))

(defun shell-extra/pre-init-vterm ()
  (spacemacs/add-to-hook 'vterm-mode-hook
                         '(xy/pretty-vterm-buffer))

  ;; (spacemacs|use-package-add-hook vterm
  ;;   :post-config
  ;;   (add-hook 'vterm-mode-hook
  ;;             (lambda()
  ;;               (setq buffer-face-mode-face '((:family "Sarasa Term SC Nerd" :height 110)))
  ;;               (buffer-face-mode)
  ;;               ;; (local-unset-key (kbd "M-<return>"))
  ;;               )))

  )

;; (defun shell-extra/pre-init-multi-term ()
;;   (spacemacs|use-package-add-hook window
;;     :pre-init
;;     (add-to-list 'display-buffer-alist
;;                  '("dedicated\\*" display-buffer-at-bottom))))

(defun shell-extra/init-aweshell ()
  (use-package aweshell
    :defer t
    :commands (aweshell-new aweshell-prev aweshell-next
                            aweshell-toggle aweshell-dedicated-toggle)
    ))

(defun shell-extra/pre-init-eshell ()
  (spacemacs|use-package-add-hook eshell
    :pre-init
    (setq eshell-rc-script (locate-user-emacs-file "eshell/profile")
          eshell-login-script (locate-user-emacs-file "eshell/login"))

    :post-config
    ;; (setq eshell-banner-message '(format "%s %s\n"
    ;;                                      (propertize (format " %s " (string-trim (buffer-name)))
    ;;                                                  'face 'mode-line-highlight)
    ;;                                      (propertize (current-time-string)
    ;;                                                  'face 'font-lock-keyword-face)))
    (setq eshell-glob-case-insensitive t
          eshell-error-if-no-glob t)

    ;; 在Emacs里输入vi，直接在buffer里打开文件
    (defalias 'eshell/vi 'find-file)
    (defalias 'eshell/vim 'find-file)
    ;; (defalias 'eshell/cat 'eshell/bat) ;; 语法高亮显示

    (require 'em-rebind)

    (require 'em-hist)
    (setq eshell-history-size 1024
          eshell-hist-ignoredups t
          eshell-save-history-on-exit t)

    (require 'em-term)
    (setq eshell-visual-commands '("top" "htop" "less" "more")
          eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show"))
          eshell-visual-options '(("git" "--help" "--paginate"))
          eshell-destroy-buffer-when-process-dies t)

    (define-key eshell-mode-map (kbd "C-d") #'eshell-delchar-or-maybe-eof)
    (define-key eshell-mode-map (kbd "C-r") #'consult-history)
    (define-key eshell-mode-map (kbd "C-l") #'eshell/clear)

    ))

;; (defun shell-extra/init-eshell-up ()
;;   (use-package eshell-up
;;     :commands (eshell-up eshell-up-peek)
;;     :config
;;     ;; to print the matching parent directory before changing to it
;;     (setq eshell-up-print-parent-dir t)
;;     ))

;; (defun shell-extra/init-eshell-git-prompt ()
;;   (use-package eshell-git-prompt
;;     :ensure t
;;     :after esh-mode
;;     :custom-face
;;     (eshell-git-prompt-multiline2-dir-face ((t (:foreground "#c09035" :bold t))))
;;     :config
;;     (eshell-git-prompt-use-theme 'multiline2)
;;     ))

;; (defun shell-extra/init-eshell-syntax-highlighting ()
;;   (use-package eshell-syntax-highlighting
;;     :ensure t
;;     :after esh-mode
;;     :hook (eshell-mode . eshell-syntax-highlighting-global-mode)
;;     :custom-face
;;     (eshell-syntax-highlighting-shell-command-face ((t (:foreground "#7cc77f" :bold t))))
;;     ))

;; (defun shell-extra/init-capf-autosuggest ()
;;   (use-package capf-autosuggest
;;     :ensure t
;;     :hook ((eshell-mode comint-mod) . capf-autosuggest-mode)
;;     ))

;; (defun shell-extra/init-esh-autosuggest ()
;;   (use-package esh-autosuggest
;;     :ensure t
;;     :init
;;     (add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
;;     ))
