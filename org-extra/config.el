;;; config.el --- Org-extra configuration File for Spacemacs
;; Time-stamp: <2023-03-17 Fri 03:44 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; Dumper


;; Variables

(spacemacs|use-package-add-hook org
  :post-init
  (add-hook 'after-save-hook #'org-redisplay-inline-images)
  (add-hook 'org-mode-hook #'toc-org-mode)
  :post-config
  (setq org-file-apps
        '(("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . system)
          ("\\.png\\'" . system)
          ("\\.jpg\\'" . system)
          ("\\.jpeg\\'" . system)
          ("\\.bmp\\'" . system)
          ("\\.svg\\'" . system)
          (directory . emacs)
          (auto-mode . emacs))))

(spacemacs|use-package-add-hook ob
  :pre-init
  (add-to-list 'org-babel-load-languages '(sqlite . t))
  (add-to-list 'org-babel-load-languages '(latex . t))
  (add-to-list 'org-babel-load-languages '(ditaa . t))
  :post-config
  (require 'ob-sqlite)
  (require 'ob-latex)
  (require 'ob-ditaa)
  (ad-activate 'org-babel-execute-src-block)
  (add-hook 'org-babel-after-execute-hook #'xy/org-babel-after-execute)
  )

(spacemacs|use-package-add-hook org-agenda
  :post-init
  (add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list))

(spacemacs|use-package-add-hook org-attach
  :post-config
  (require 'org-attach-git)
  ;; acctach from dired
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map
                          (kbd "C-c C-x a")
                          #'org-attach-dired-to-subtree))))

(spacemacs|use-package-add-hook org-download
  :post-init
  (add-hook 'dired-mode-hook 'org-download-enable))

(spacemacs|use-package-add-hook org-ref
  :post-config
  (setq org-ref-open-pdf-function
        (lambda (fpath)
          (start-process "zathura"
                         "*bibtex-zathura*" ;; was "*helm-bibtex-zathura*", changed because helm was removed
                         "/usr/bin/zathura" fpath)))
  (setq org-ref-bibliography-notes "~/org/ref_notes.org"
        org-ref-default-bibliography '("~/org/bib/all.bib")
        org-ref-pdf-directory "~/doc")
  (setq reftex-default-bibliography '("~/org/bib/all.bib")))

;; layer: markdown
;; (with-eval-after-load "markdown"
;;   (add-hook 'markdown-mode-hook #'toc-org-mode))
(spacemacs|use-package-add-hook markdown
  :post-init
  (add-hook 'markdown-mode-hook #'toc-org-mode))

(spacemacs|use-package-add-hook org-roam
  :post-init
  (add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list)
  :post-config
  (setq org-roam-v2-ack t
        org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-db-autosync-mode 1))

(spacemacs|use-package-add-hook org-roam-ui
  :post-init
  (spacemacs|diminish org-roam-ui-mode " ⓤ" " u")
  (spacemacs|diminish org-roam-ui-follow-mode))
