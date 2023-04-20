;;; config.el --- latex-extra configuration File for Spacemacs
;; Time-stamp: <2023-04-20 Thu 02:51 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; Put all bibliography and references settings for all packages here to avoid conflicts
(setq reftex-default-bibliography '("~/org/bib/all.bib"))
(setq bibtex-completion-bibliography '("~/org/bib/all.bib")
      bibtex-completion-library-path '("~/doc/")
      bibtex-completion-notes-path "~/org/roam/")
;; It seems org-ref don't need these vars any more.
;; (setq org-ref-bibliography-notes "~/org/ref_notes.org"
;;       org-ref-default-bibliography '("~/org/bib/all.bib")
;;       org-ref-pdf-directory "~/doc")

(spacemacs|use-package-add-hook bibtex
  :post-config
  (setq bibtex-autokey-name-year-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-year-length 4
        bibtex-autokey-year-title-separator "-"))

(spacemacs|use-package-add-hook bibtex-completion
  :post-config
  (setq bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
        '((article . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        bibtex-completion-notes-template-multiple-files
        "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \12\12See [[cite:&${=key=}]]\12"
        bibtex-completion-pdf-open-function
        '(closure (t) (fpath) (call-process "open" nil 0 nil fpath))))

(with-eval-after-load 'tex
  (setq LaTeX-command-style
        '(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %(output-dir) %S%(PDFout)"))))
