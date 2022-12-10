(load-file "~/.emacs.d/private/efs/efs-defaults.el")

(use-package treemacs)

(treemacs)

(use-package markdown-mode)

(require 'markdown-mode)

(use-package posframe)

(require 'posframe)

(add-to-list 'load-path "~/src/lsp-bridge")

(require 'lsp-bridge)
;; Test with my pylsp.json, not working
;; (setq lsp-bridge-python-lsp-server "pylsp")

(global-lsp-bridge-mode)

(use-package dumb-jump)

;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
(defun lsp-bridge-jump ()
  (Interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (function-called-at-point)))
      (when symb
        (find-function symb))))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun lsp-bridge-jump-back ()
  (interactive)
  (cond
   (lsp-bridge-mode
    (lsp-bridge-return-from-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))
