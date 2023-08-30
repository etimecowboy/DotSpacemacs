;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- latex-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-08-30 Wed 06:42 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq latex-extra-packages
      '(cdlatex
        bibtex
        bibtex-completion ;; implicitly required by the bibtex layer
        reftex
        auctex
        (consult-bibtex
         :require (consult embark)
         :location (recipe :fetcher github :repo "mohkale/consult-bibtex"))
        embark
        ))

(defun latex-extra/init-cdlatex ()
  (spacemacs|use-package-add-hook org :post-config (require 'cdlatex))
  (spacemacs|use-package-add-hook tex :post-config (require 'cdlatex))
  (use-package cdlatex
    :defer t
    :hook
    ;; (org-mode . turn-on-org-cdlatex)
    (LaTeX-mode . turn-on-org-cdlatex)
    :config
    (spacemacs|diminish org-cdlatex-mode " Ⓒ" " C")
    ))

(defun latex-extra/pre-init-bibtex ()
  (spacemacs|use-package-add-hook bibtex
    :post-config
    (setq bibtex-autokey-name-year-separator "-"
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-titlewords 2
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-year-length 4
          bibtex-autokey-year-title-separator "-")
    ))

(defun latex-extra/init-bibtex-completion ()
  (use-package bibtex-completion
    :defer t
    :config
    (setq bibtex-completion-bibliography '("~/org/bib/all.bib")
          bibtex-completion-library-path '("~/doc/")
          bibtex-completion-notes-path "~/org/roam/"
          bibtex-completion-additional-search-fields '(keywords)
          bibtex-completion-display-formats
          '((article . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
            (inbook . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
            (incollection . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
            (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
            (t . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
          bibtex-completion-notes-template-multiple-files
          "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \12\12See [[cite:&${=key=}]]\12"
          bibtex-completion-pdf-open-function '(closure (t) (fpath) (call-process "open" nil 0 nil fpath)))

    ;; define a new citation format for org-ref
    (defun bibtex-completion-format-citation-org-ref (keys)
      "Format org-links using org-ref's own cite syntax."
      (format "[[cite:%s]]"
              (s-join ";"
                      (--map (format "&%s" it) keys))))

    (setq bibtex-completion-format-citation-functions
          '((org-mode      . bibtex-completion-format-citation-org-ref)
            (latex-mode    . bibtex-completion-format-citation-cite)
            (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
            (python-mode   . bibtex-completion-format-citation-sphinxcontrib-bibtex)
            (rst-mode      . bibtex-completion-format-citation-sphinxcontrib-bibtex)
            (default       . bibtex-completion-format-citation-default)))
    ))


(defun latex-extra/pre-init-reftex ()
  (spacemacs|use-package-add-hook reftex
    :pre-init
    ;; Put all bibliography and references settings for all packages here to avoid conflicts
    (setq reftex-default-bibliography '("~/org/bib/all.bib"))
  ))


(defun latex-extra/pre-init-auctex ()
  (spacemacs|use-package-add-hook auctex
    :post-config
    (setq LaTeX-command-style
          '(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %(output-dir) %S%(PDFout)")))
  ))


(defun latex-extra/init-consult-bibtex ()
  (use-package consult-bibTeX
    :defer t
    :bind (("M-s B" . consult-bibtex))
    ))


(defun latex-extra/pre-init-embark ()
  (spacemacs|use-package-add-hook embark
    :post-config
    (require 'bibtex-completion)
    (require 'consult-bibtex-embark)
    (add-to-list 'embark-keymap-alist
                 '(bibtex-completion . consult-bibtex-embark-map))
    ;;
    ;; Add a embark action for inserting `org-ref' citation;
    ;; NOT as good as define and add one in `bibtex-completion-format-citation-functions' 
    ;; `bibtex-completion'
    ;;
    ;; (consult-bibtex-embark-action consult-bibtex-insert-org-ref-citation
    ;;                               org-ref-insert-cite-link)
    ;; (setq consult-bibtex-embark-map
    ;;   (let ((map (make-sparse-keymap)))
    ;;     (define-key map "o" #'consult-bibtex-open-pdf)
    ;;     (define-key map "u" #'consult-bibtex-open-url-or-doi)
    ;;     (define-key map "c" #'consult-bibtex-insert-citation)
    ;;     (define-key map "r" #'consult-bibtex-insert-reference)
    ;;     (define-key map "k" #'consult-bibtex-insert-key)
    ;;     (define-key map "b" #'consult-bibtex-insert-bibtex)
    ;;     (define-key map "a" #'consult-bibtex-add-PDF-attachment)
    ;;     (define-key map "e" #'consult-bibtex-edit-notes)
    ;;     (define-key map "j" #'consult-bibtex-insert-pdftools-link)
    ;;     (define-key map "s" #'consult-bibtex-show-entry)
    ;;     (define-key map "l" #'consult-bibtex-add-pdf-to-library)
    ;;     (define-key map "r" #'consult-bibtex-insert-org-ref-citation)
    ;;     map))
    ))
