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
  '(;; vterm
    multi-vterm ;; included in shell layer
    eshell
    (aweshell :location (recipe :fetcher github :repo "manateelazycat/aweshell"))
    ;; eshell-git-prompt
    ;; eshell-syntax-highlighting
    ;; capf-autosuggest
    ;; edh-autosuggest
    ;; eshell-up
    ))

;; (defun shell-extra/pre-init-vterm ()
;;   (spacemacs/add-to-hook 'vterm-mode-hook
;;                          '(xy/adapt-shell-config))
;;   ;; (add-hook 'vterm-mode-hook
;;   ;;           (lambda()
;;   ;;             (setq buffer-face-mode-face
;;   ;;                   '((:family "Sarasa Term SC Nerd" :height 110)))
;;   ;;             (buffer-face-mode)
;;   ;;             ;; (local-unset-key (kbd "M-<return>"))
;;   ;;             )))
;;   ;; (spacemacs|use-package-add-hook vterm
;;   ;;   :post-config
;;   ;;   (when window-system
;;   ;;     (setq vterm-shell "tmux new-session -A -s default")))
;;   )

;; (defun shell-extra/post-init-vterm ()
;;   (when window-system
;;     (setq vterm-shell "tmux new-session -A -s default")))


;; (defun shell-extra/pre-init-multi-vterm ()
;;   (spacemacs|use-package-add-hook multi-vterm
;;     :post-config
;;     ;; REF: https://github.com/suonlight/multi-vterm/issues/23
;;     ;; (add-to-list 'display-buffer-alist
;;     ;;              '("dedicated.*$" display-buffer-at-bottom))
;;     ;; (setq display-buffer-alist
;;     ;;       '(("^\\*vterminal.*$" display-buffer-at-bottom)))
;;     ;; override default multi-vterm
;;     ;; (defun multi-vterm ()
;;     ;;   "Create new vterm buffer."
;;     ;;   (interactive)
;;     ;;   (let* ((vterm-buffer (multi-vterm-get-buffer)))
;;     ;;     (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
;;     ;;     (set-buffer vterm-buffer)
;;     ;;     (multi-vterm-internal)
;;     ;;     (pop-to-buffer-same-window vterm-buffer)))

;;     ;; (when window-system
;;     ;;   ;; (setq-default multi-vterm-program "tmux new-session -A -s default")
;;     ;;   (setq multi-vterm-program "tmux new-session -A -s default")
;;     ;;   (setq vterm-shell "tmux new-session -A -s default"))

;;     (setq multi-vterm-dedicated-window-height-percent 40)
;;     ))

(defun shell-extra/post-init-multi-vterm ()
  ;; (when window-system
  ;;   ;; (setq-default multi-vterm-program "tmux new-session -A -s default")
  ;;   (setq multi-vterm-program "tmux new-session -A -s default")
  ;;   (setq vterm-shell "tmux new-session -A -s default"))
  (setq multi-vterm-dedicated-window-height-percent 40))

(defun shell-extra/init-aweshell ()
  (use-package aweshell
    :defer t
    :commands (aweshell-new aweshell-prev aweshell-next
                            aweshell-toggle aweshell-dedicated-toggle)
    ))

(defun shell-extra/pre-init-eshell ()
  (spacemacs|use-package-add-hook eshell
    ;; (spacemacs/add-to-hook 'eshell-alias-load-hook
    ;;                        '(eshell-load-bash-aliases))
    :pre-init
    (setq eshell-rc-script (locate-user-emacs-file "eshell/profile")
          eshell-login-script (locate-user-emacs-file "eshell/login"))

    :post-config
    ;; NOTE: displaying a banner is not good for pop-up eshell buffers.
    ;;
    ;; (setq eshell-banner-message '(format "%s %s\n"
    ;;                                      (propertize (format " %s " (string-trim (buffer-name)))
    ;;                                                  'face 'mode-line-highlight)
    ;;                                      (propertize (current-time-string)
    ;;                                                  'face 'font-lock-keyword-face)))
    (setq eshell-glob-case-insensitive t
          eshell-error-if-no-glob t)

    ;; Add aliases
    (defalias 'eshell/vi 'find-file)
    (defalias 'eshell/vim 'find-file)
    (defalias 'eshell/emacs 'find-file)
    (defalias 'eshell/cat 'eshell/bat) ;; 语法高亮显示
    ;; (defalias 'eshell/li '(shell-command "exa -alFh --icons"))
    ;; (defalias 'eshell/lg '(shell-command "exa -alFh --icons --git"))

    (require 'em-rebind)

    (require 'em-hist)
    (setq eshell-history-size 10240
          eshell-hist-ignoredups t
          eshell-save-history-on-exit t)

    (require 'em-term)
    (setq eshell-visual-commands '("top" "htop" "less" "more")
          eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show"))
          eshell-visual-options '(("git" "--help" "--paginate"))
          eshell-destroy-buffer-when-process-dies t)

    (define-key eshell-mode-map (kbd "C-d") #'eshell-delchar-or-maybe-eof)
    (if (featurep 'consult) (define-key eshell-mode-map (kbd "C-r") #'consult-history))
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
