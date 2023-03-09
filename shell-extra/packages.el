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
  '(;; multi-vterm ;; included in shell layer
    (aweshell :location (recipe :fetcher github :repo "manateelazycat/aweshell"))
    ))


;; (defun shell-extra/init-multi-vterm ()
;;   (use-package multi-vterm
;;     :after vterm))

(defun shell-extra/init-aweshell ()
  (use-package aweshell
    :defer t
    :commands (aweshell-new aweshell-prev aweshell-next
                            aweshell-toggle aweshell-dedicated-toggle)
    ))
