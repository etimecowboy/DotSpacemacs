;;; packages.el --- lsp-bridge Layer packages File for Spacemacs
;; Time-stamp: <2023-12-16 Sat 10:19 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This layer add another lsp client - lsp-bridge
;;
;;; Code:

(defconst lsp-bridge-packages
      '(
        ;; (lsp-bridge :location (recipe
        ;;                        :fetcher github
        ;;                        :repo "manateelazycat/lsp-bridge"
        ;;                        :files ("*") ;;  "acm/*" "core/*")
        ;;                        ))
        ;; FIXME: cause error after lsp-bridge updated after 2023-08-01
        ;;
        ;; Error in post-command-hook (lsp-bridge-monitor-post-command):
        ;; (invalid-function acm-with-cache-candidates)
        ;;
        ;; (lsp-bridge :location (recipe :fetcher local)) ;; FIXME: same error
        (lsp-bridge :location local)
        ;; local git repo works fine ./local/lsp-bridge -> ~/src/lsp-bridge
        (popon :location (recipe
                          :fetcher git
                          :url "https://codeberg.org/akib/emacs-popon.git"))
        (acm-terminal :location (recipe
                                 :fetcher github
                                 :repo "twlz0ne/acm-terminal"))
        yasnippet
        yasnippet-snippets
        ))

(defun lsp-bridge/init-lsp-bridge ()
  (use-package lsp-bridge
    :defer t
    ;; :init
    ;; (add-hook 'lsp-bridge-mode
    ;;           (lambda () (text-scale-set -1)))
    :hook
    (prog-mode . lsp-bridge-mode)
    (prog-mode . (lambda () (text-scale-set -1)))
    ;; (org-mode . lsp-bridge-mode)
    ;; (latex-mode . lsp-bridge-mode)
    ;; (LaTeX-mode . lsp-bridge-mode)
    ;; (TeX-latex-mode . lsp-bridge-mode)
    ;; ;; (latex-ts-mode . lsp-bridge-mode)
    ;; (sh-mode . lsp-bridge-mode)
    ;; (bash-ts-mode . lsp-bridge-mode)
    ;; (python-mode . lsp-bridge-mode)
    ;; (python-ts-mode . lsp-bridge-mode)
    ;; (emacs-lisp-mode . lsp-bridge-mode)
    ;; (lisp-interaction-mode . lsp-bridge-mode)
    ;; (c-mode . lsp-bridge-mode)
    ;; ;; (c-ts-mode . lsp-bridge-mode)
    ;; (c++-mode . lsp-bridge-mode)
    ;; ;; (c++-ts-mode . lsp-bridge-mode)
    ;; (rust-mode . lsp-bridge-mode)
    ;; ;; (rust-ts-mode . lsp-bridge-mode)
    ;; (lua-mode . lsp-bridge-mode)
    ;; ;; (lua-ts-mode . lsp-bridge-mode)
    :init
    ;; (setq lsp-bridge-dir (file-name-directory (locate-library "lsp-bridge")))
    ;; ;; (setq lsp-bridge-dir "~/src/lsp-bridge/")
    ;; (add-to-list 'load-path lsp-bridge-dir)
    ;; (add-to-list 'load-path (concat lsp-bridge-dir "core/"))
    ;; (add-to-list 'load-path (concat lsp-bridge-dir "acm/"))

    (spacemacs|define-transient-state lsp-bridge
      :title "lsp-bridge transient state"
      :doc "
^Act^                     ^Find^                                 ^Doc^
^^^^^^^^----------------------------------------------------------------------------------
[_SPC_] complete menu     [_d_/_D_] definition/other window      [_?_] documentation
[_E_] toggle sdcv helper  [_i_/_I_] implementation/other window  [_C-v_/_j_] scroll up
[_P_] peek                [_t_/_T_] type/other window            [_M-v_/_k_] scroll down
[_R_] restart             [_r_] find reference
[_q_] quit
"
      :bindings
      ("q" nil :exit t)
      ("SPC" lsp-bridge-popup-complete-menu)
      ("E" lsp-bridge-toggle-sdcv-helper :exit t)
      ("R" lsp-bridge-restart-process)
      ("p" lsp-bridge-peek)
      ("P" lsp-bridge-peek-jump)
      ("B" lsp-bridge-peek-jump-back)
      ("T" lsp-bridge-peek-through)
      ("F" lsp-bridge-code-format)
      ("?" lsp-bridge-popup-documentation)
      ("C-v" lsp-bridge-popup-documentation-scroll-up)
      ("j" lsp-bridge-popup-documentation-scroll-up)
      ("M-v" lsp-bridge-popup-documentation-scroll-down)
      ("k" lsp-bridge-popup-documentation-scroll-down)
      ("d" lsp-bridge-find-def)
      ("D" lsp-bridge-find-def-other-window)
      ("i" lsp-bridge-find-impl)
      ("I" lsp-bridge-find-impl-other-window)
      ("t" lsp-bridge-find-type-def)
      ("T" lsp-bridge-find-type-def-other-window)
      ("r" lsp-bridge-find-references)
      )

    :config

    ;; activate conda virtual environment to run lsp servers.
    (require 'conda)
    (conda-env-activate "py310_emacs")

    (require 'acm)
    (require 'acm-terminal)
    (setq lsp-bridge-python-lsp-server "pyright_ruff"
          lsp-bridge-c-lsp-server "ccls"
          lsp-bridge-tex-lsp-server "texlab"
          ;; NOTE: To enable lsp-bridge in org-babel source blocks,
          ;; You have to add the major mode to `org-src-lang-modes'
          ;; REF: https://emacs-china.org/t/lsp-bridge/20786/3130
          lsp-bridge-enable-org-babel t
          lsp-bridge-enable-completion-in-string t
          ;; lsp-bridge-use-ds-pinyin-in-org-mode nil
          lsp-bridge-enable-completion-in-minibuffer t
          lsp-bridge-enable-hover-diagnostic t
          ;; acm-enable-tabnine nil
          ;; acm-enable-path nil
          lsp-bridge-code-action-preview-delay 1.0
          )
    (setq acm-enable-preview nil
          acm-enable-quick-access t
          acm-quick-access-modifier 'control
          acm-backend-search-file-words-max-number 15
          )

    (spacemacs|diminish lsp-bridge-mode " ⓠ" " q")
    ;; (spacemacs|diminish lsp-bridge-mode " 橋" " q")

    ;; (unless (display-graphic-p)
    ;;   (with-eval-after-load 'acm
    ;;     (require 'acm-terminal)))
    ;; (with-eval-after-load 'acm
    ;;   (unless (display-graphic-p)
    ;;     (require 'acm-terminal)))

    ;; NOTE: lsp server may missing
    ;; (global-lsp-bridge-mode)
    ))

(defun lsp-bridge/init-popon ()
  (use-package popon
    :defer t
    ))

(defun lsp-bridge/init-acm-terminal ()
  (use-package acm-terminal
    :defer t
    ))

(defun lsp-bridge/init-yasnippet ()
  (use-package yasnippet
    :ensure t
    :init
    (defvar yas-snippet-dirs nil)
    (setq auto-completion-private-snippets-directory "/home/xin/src/spacemacs/private/snippets")
    (add-to-list 'yas-snippet-dirs 'auto-completion-private-snippets-directory)
    :config
    (spacemacs|diminish yas-minor-mode " ⓨ" " y")
    (yas-global-mode t)
    ))

(defun lsp-bridge/init-yasnippet-snippets ())
