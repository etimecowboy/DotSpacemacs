;;; packages.el --- sunrise-commander packages File for Spacemacs
;; Time-stamp: <2021-03-12 Fri 14:03 by xin on legion>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq sunrise-commander-packages
  '(
    (sunrise-commander
     :location (recipe :fetcher github
                       :repo "sunrise-commander/sunrise-commander"))
    ))

(defun sunrise-commander/init-sunrise-commander ()
  (use-package sunrise
    :defer t
    )
  (use-package sunrise-popviewer
    :defer t
    )
  (use-package sunrise-checkpoints
    :defer t
    )
  (use-package sunrise-loop
    :defer t
    )
  (use-package sunrise-mirror
    :defer t
    )
  (use-package sunrise-tabs
    :defer t
    )
  (use-package sunrise-modeline
    :defer t
   )
  )
