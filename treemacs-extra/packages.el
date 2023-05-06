;;; packages.el --- treemacs-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-05-06 Sat 06:48 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst treemacs-extra-packages '(treemacs))

(defun treemacs-extra/pre-init-treemacs ()
  (spacemacs|use-package-add-hook treemacs
    :post-init
    ;; REF: https://github.com/Alexander-Miller/treemacs/issues/842
    (add-hook 'treemacs-mode-hook
              #'(lambda ()
                  ;; (message "treemacs-mode-hook `%s'" (current-buffer))
                  ;; NOTE: Treemacs buffer in terminal mode cannot display
                  ;; scaled text.
                  (when window-system
                    (text-scale-decrease 1))))
    :post-config
    (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace)
    (treemacs-define-RET-action 'file-node-open #'treemacs-visit-node-ace)
    ))
