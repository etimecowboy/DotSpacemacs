;;; packages.el --- treemacs-extra layer packages file for Spacemacs.
;; Time-stamp: <2024-04-01 Mon 03:08 by xin on tufg>
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
    (add-hook 'treemacs-mode-hook #'xy--treemacs-window-setup)

    :post-config
    (defun xy--treemacs-window-setup ()
      ;; dynamic workspace title
      ;; REF: https://andreyor.st/posts/2020-05-01-dynamic-title-for-treemacs-workspace/
      (let ((bg (face-attribute 'default :background))
            (fg (face-attribute 'default :foreground)))
        (face-remap-add-relative 'header-line
                                 :background bg :foreground fg
                                 :box `(:line-width ,(/ (line-pixel-height) 2) :color ,bg)))
      (setq header-line-format
            '((:eval
               (let* ((text (treemacs-workspace->name (treemacs-current-workspace)))
                      (extra-align (+ (/ (length text) 2) 1))
                      (width (- (/ (window-width) 2) extra-align)))
                 (concat (make-string width ?\s) text)))))

      ;; smaller font size in graphic mode
      (when window-system
        (text-scale-decrease 1)))

    (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace)
    (treemacs-define-RET-action 'file-node-open #'treemacs-visit-node-ace)
    ))
