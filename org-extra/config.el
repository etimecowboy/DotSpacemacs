;;; config.el --- Org-extra configuration File for Spacemacs
;; Time-stamp: <2023-02-19 Sun 15:20 by xin on tufg>
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

;; Timestamp on babel-execute results block
;; REF: https://emacs.stackexchange.com/questions/16850/timestamp-on-babel-execute-results-block

;; Examples:
;; #+NAME: test-no-timestamp
;; #+BEGIN_SRC shell :results output
;; echo "This ones doesn't have the right args for timestamping"
;; #+END_SRC

;; #+RESULTS: test-no-timestamp
;; : This ones doesn't have the right args for timestamping

;; #+NAME: test-timestamp
;; #+BEGIN_SRC shell :results output :timestamp t
;; echo "This one should have a timestamp. Run me again, I update."
;; #+END_SRC

;; #+RESULTS[2017-10-03 05:19:09 AM]: test-timestamp
;; : This one should have a timestamp. Run me again, I update.

(defadvice org-babel-execute-src-block (after org-babel-record-execute-timestamp)
  (let ((code-block-params (nth 2 (org-babel-get-src-block-info)))
        (code-block-name (nth 4 (org-babel-get-src-block-info))))
    (let ((timestamp (cdr (assoc :timestamp code-block-params)))
          (result-params (assoc :result-params code-block-params)))
      (if (and (equal timestamp "t") (> (length code-block-name) 0))
          (save-excursion
            (search-forward-regexp (concat "#\\+RESULTS\\(\\[.*\\]\\)?: "
                                           code-block-name))
            (beginning-of-line)
            (search-forward "RESULTS")
            (kill-line)
            (insert (concat (format-time-string "[%F %r]: ") code-block-name)))
        (if (equal timestamp "t")
            (message (concat "Result timestamping requires a #+NAME: "
                             "and a ':results output' argument.")))))))

(with-eval-after-load "org"
  (ad-activate 'org-babel-execute-src-block)
  (add-hook 'org-babel-after-execute-hook #'xy/org-babel-after-execute)
  (add-hook 'after-save-hook #'org-redisplay-inline-images)
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
          (auto-mode . emacs)
          )))

;; FIXME: failed try
;; (spacemacs|use-package-add-hook "org"
;;   :post-config
;;   (ad-activate 'org-babel-execute-src-block)
;;   (add-hook 'org-babel-after-execute-hook #'xy/org-babel-after-execute)
;;   (add-hook 'after-save-hook #'org-redisplay-inline-images)
;;   (setq org-file-apps
;;         '(("\\.mm\\'" . default)
;;           ("\\.x?html?\\'" . default)
;;           ("\\.pdf\\'" . system)
;;           ("\\.png\\'" . system)
;;           ("\\.jpg\\'" . system)
;;           ("\\.jpeg\\'" . system)
;;           ("\\.bmp\\'" . system)
;;           ("\\.svg\\'" . system)
;;           (directory . emacs)
;;           (auto-mode . emacs))))

;;; package: org-attach
(with-eval-after-load "org-attach"
  (require 'org-attach-git)
  ;; acctach from dired
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map
                (kbd "C-c C-x a")
                #'org-attach-dired-to-subtree))))

;;; package: org-download
(with-eval-after-load "org-download"
  (add-hook 'dired-mode-hook 'org-download-enable))

;;; package: toc-org
(with-eval-after-load "org"
  (add-hook 'org-mode-hook #'toc-org-mode))

;; FIXME: try to solve cannot complete org-roam nodes
;; (add-hook 'org-mode-hook #'org-roam-update-org-id-locations) ;; too slow
;; (add-hook 'org-mode-hook #'org-roam-node-read--completions)
;; (add-hook 'org-mode-hook #'org-roam-buffer-refresh)
;; FIXME: Reload local settings when org file headings changed
;; (add-hook 'after-save-hook #'org-mode-restart)

;; (with-eval-after-load "org-roam"
;;   (add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list)
;;   (setq org-roam-v2-ack t
;;         org-roam-db-gc-threshold most-positive-fixnum)
;;   (org-roam-db-autosync-mode 1))
;; ;; (xy/load-lob) ;; <2022-11-17 Thu> FIXME: caused org-mode font-locking problem

;; FIXME: failed try
;; (spacemacs|use-package-add-hook "org-roam"
;;   :post-config
;;   (add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list)
;;   (setq org-roam-v2-ack t
;;         org-roam-db-gc-threshold most-positive-fixnum)
;;   (org-roam-db-autosync-mode 1))

;; (with-eval-after-load "org-roam-ui"
;;   (spacemacs|diminish org-roam-ui-mode " Ⓤ" " U")
;;   (spacemacs|diminish org-roam-ui-follow-mode))

;; FIXME: failed try
;; (spacemacs|use-package-add-hook "org-roam-ui"
;;   :post-config
;;   (spacemacs|diminish org-roam-ui-mode " Ⓤ" " U")
;;   (spacemacs|diminish org-roam-ui-follow-mode))

;; layer: bibtex
;;; package: org-ref
(with-eval-after-load "org-ref"
  (setq org-ref-open-pdf-function
        (lambda (fpath)
          (start-process "zathura"
                         "*bibtex-zathura*" ;; was "*helm-bibtex-zathura*", changed because helm was removed
                         "/usr/bin/zathura" fpath)))
  (setq org-ref-bibliography-notes "~/org/ref_notes.org"
        org-ref-default-bibliography '("~/org/bib/all.bib")
        org-ref-pdf-directory "~/doc")
  (setq reftex-default-bibliography '("~/org/bib/all.bib")))

;; FIXME: failed try
;; (spacemacs|use-package-add-hook "org-ref"
;;   :post-config
;;   (setq org-ref-open-pdf-function
;;         (lambda (fpath)
;;           (start-process "zathura"
;;                          "*bibtex-zathura*" ;; was "*helm-bibtex-zathura*", changed because helm was removed
;;                          "/usr/bin/zathura" fpath)))
;;   (setq org-ref-bibliography-notes "~/org/ref_notes.org"
;;         org-ref-default-bibliography '("~/org/bib/all.bib")
;;         org-ref-pdf-directory "~/doc")
;;   (setq reftex-default-bibliography '("~/org/bib/all.bib")))

;; layer: markdown
(with-eval-after-load "markdown"
  (add-hook 'markdown-mode-hook #'toc-org-mode))

;; ;; load sqlite
;; (with-eval-after-load 'org
;;   (require 'ob-sqlite)
;;   (add-to-list 'org-babel-load-languages '(sqlite . t)))

;; ;; load latex
;; (with-eval-after-load 'org
;;   (require 'ob-latex)
;;   (add-to-list 'org-babel-load-languages '(latex . t)))

;; FIXME: failed try
;; (spacemacs|use-package-add-hook "org"
;;   :post-config
;;   (add-to-list 'org-babel-load-languages '(latex . t))
;;   (add-to-list 'org-babel-load-languages '(sqlite . t))
;;   )
