;;; config.el --- treemacs-extra configuration File for Spacemacs
;; Time-stamp: <2023-03-12 Sun 01:25 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

(with-eval-after-load "treemacs"
  ;; FIXME: opens/closes files using ace, not functioning after update.
  ;; (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace)
  ;; (treemacs-define-RET-action 'file-node-open #'treemacs-visit-node-ace)

  ;; REF: https://github.com/Alexander-Miller/treemacs/issues/842
  (add-hook 'treemacs-mode-hook
            #'(lambda ()
                ;; (message "treemacs-mode-hook `%s'" (current-buffer))
                ;; NOTE: Treemacs buffer in terminal mode cannot display
                ;; scaled text.
                (when window-system
                  (text-scale-decrease 1)))))
