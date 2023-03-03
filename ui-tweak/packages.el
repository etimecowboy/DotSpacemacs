;;; packages.el --- ui packages File for Spacemacs
;;
;; Copyright (c) 2021-2023 Xin Yang
;;
;; Author: Xin Yang <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; (defconst ui-tweak-packages
;;   '(;; mini-frame ;; not compatible with embark
;;     ;; (mini-modeline :location (recipe :fetcher github :repo "kiennq/emacs-mini-modeline")) ;; not compatible with eyebrowse or persp-mode
;;     ))


;; (defun ui-tweak/init-mini-frame ()
;;   (use-package mini-frame
;;     :defer t
;;     :config
;;     (setq ;; mini-frame-advice-functions
;;           ;; '(read-from-minibuffer read-string yes-or-no-p
;;           ;;                        spacemacs/helm-M-x-fuzzy-matching helm-M-x
;;           ;;                        spacemacs/helm-find-files helm-find-files
;;           ;;                        helm-buffers-list
;;           ;;                        )
;;           mini-frame-show-parameters
;;             '((top . 10)
;;               (width . 0.7)
;;               (left . 0.5)
;;               ;;(height . 15)
;;               )
;;           ;; mini-frame-completions-focus 'completions
;;           ;; mini-frame-resize-max-height 25
;;           ;; mini-frame-resize-min-height 5
;;           ;; mini-frame-standalone t
;;           )
;;     ))

;; (defun ui-tweak/init-mini-modeline ()
;;   (use-package mini-modeline
;;     ;;:after smart-mode-line
;;     :after spaceline
;;     :config
;;     (setq mini-modeline-truncate-p nil)
;;     ;;(mini-modeline-mode t)
;;     )
;;   )
