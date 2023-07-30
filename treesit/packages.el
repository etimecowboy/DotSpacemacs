;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- treesit layer packages file for Spacemacs.
;; Time-stamp: <2023-07-29 Sat 07:00 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst treesit-packages
  '(
    treesit-auto
    ;; (fingertip :location (recipe
    ;;                       :fetcher github
    ;;                       :repo "manateelazycat/fingertip"))
    ))

(defun treesit/init-treesit-auto ()
  "Initialize treesit-auto"
  (use-package treesit-auto
    :demand t
    :config
    ;; elisp
    (setq my-elisp-tsauto-config
	        (make-treesit-auto-recipe
	         :lang 'elisp
	         :ts-mode 'elisp-ts-mode
	         :remap '(emacs-lisp lisp-interaction-mode)
	         :url "https://github.com/Wilfred/tree-sitter-elisp"
	         :revision "main"
	         :source-dir "src"))
    (add-to-list 'treesit-auto-recipe-list my-elisp-tsauto-config)
    ;; (add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
    ;; (add-hook 'lisp-interaction-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
    ;; sql
    (setq my-sql-tsauto-config
	        (make-treesit-auto-recipe
	         :lang 'sql
	         :ts-mode 'sql-ts-mode
	         :remap 'sql-mode
	         :url "https://github.com/m-novikov/tree-sitter-sql"
	         :revision "main"
	         :source-dir "src"))
    (add-to-list 'treesit-auto-recipe-list my-sql-tsauto-config)
    ;; org
    (setq my-org-tsauto-config
	        (make-treesit-auto-recipe
	         :lang 'org
	         :ts-mode 'org-ts-mode
	         :remap 'org-mode
	         :url "https://github.com/milisims/tree-sitter-org"
	         :revision "main"
	         :source-dir "src"))
    (add-to-list 'treesit-auto-recipe-list my-org-tsauto-config)

    ;; treesit
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode)
    ))

;; (defun treesit-extra/init-fingertip ()
;;   (use-package fingertip
;;     :init
;;     (dolist (hook (list
;;                    'c-mode-common-hook
;;                    'c-mode-hook
;;                    'c++-mode-hook
;;                    ;; 'java-mode-hook
;;                    ;; 'haskell-mode-hook
;;                    ;; 'emacs-lisp-mode-hook
;;                    ;; 'lisp-interaction-mode-hook
;;                    ;; 'lisp-mode-hook
;;                    ;; 'maxima-mode-hook
;;                    ;; 'ielm-mode-hook
;;                    'sh-mode-hook
;;                    'makefile-gmake-mode-hook
;;                    ;; 'php-mode-hook
;;                    'python-mode-hook
;;                    ;; 'js-mode-hook
;;                    ;; 'go-mode-hook
;;                    ;; 'qml-mode-hook
;;                    ;; 'jade-mode-hook
;;                    ;; 'css-mode-hook
;;                    ;; 'ruby-mode-hook
;;                    ;; 'coffee-mode-hook
;;                    'rust-mode-hook
;;                    'rust-ts-mode-hook
;;                    ;; 'qmake-mode-hook
;;                    ;; 'lua-mode-hook
;;                    ;; 'swift-mode-hook
;;                    ;; 'web-mode-hook
;;                    ;; 'markdown-mode-hook
;;                    ;; 'llvm-mode-hook
;;                    ;; 'conf-toml-mode-hook
;;                    ;; 'nim-mode-hook
;;                    ;; 'typescript-mode-hook
;;                    'c-ts-mode-hook
;;                    'c++-ts-mode-hook
;;                    'cmake-ts-mode-hook
;;                    ;; 'toml-ts-mode-hook
;;                    ;; 'css-ts-mode-hook
;;                    ;; 'js-ts-mode-hook
;;                    'json-ts-mode-hook
;;                    'python-ts-mode-hook
;;                    'bash-ts-mode-hook
;;                    ;; 'typescript-ts-mode-hook
;;                    'latex-mode-hook
;;                    'latex-ts-mode-hook
;;                    ))
;;       (add-hook hook #'(lambda () (fingertip-mode 1))))
;;     :bind
;;     (:map fingertip-mode-map
;;           ("(" . fingertip-open-round)
;;           ("[" . fingertip-open-bracket)
;;           ("{" . fingertip-open-curly)
;;           (")" . fingertip-close-round)
;;           ("]" . fingertip-close-bracket)
;;           ("}" . fingertip-close-curly)
;;           ("=" . fingertip-equal)

;;           ("%" . fingertip-match-paren)
;;           ("\"" . fingertip-double-quote)
;;           ("'" . fingertip-single-quote)

;;           ("SPC" . fingertip-space)
;;           ("RET" . fingertip-newline)

;;           ("M-o" . fingertip-backward-delete)
;;           ("C-d" . fingertip-forward-delete)
;;           ("C-k" . fingertip-kill)

;;           ("M-\"" . fingertip-wrap-double-quote)
;;           ("M-'"  . fingertip-wrap-single-quote)
;;           ("M-["  . fingertip-wrap-bracket)
;;           ("M-{"  . fingertip-wrap-curly)
;;           ("M-("  . fingertip-wrap-round)
;;           ("M-)"  . fingertip-unwrap)

;;           ("M-p"  . fingertip-jump-right)
;;           ("M-n"  . fingertip-jump-left)
;;           ("M-:"  . fingertip-jump-out-pair-and-newline)

;;           ("C-j"  . fingertip-jump-up))
;;     ))
