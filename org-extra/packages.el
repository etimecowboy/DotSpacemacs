;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- org-extra layer packages file for Spacemacs.
;; Time-stamp: <2022-03-10 Thu 00:38 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq org-extra-packages
      '(
        ;; (org-crypt :location built-in)
	      ;; (org-attach :location built-in)
        ob-async
        ;; ob-restclient ;; owned in restclient layer
        ob-ipython
        ;; (ob-plantuml :location built-in)
        org-pdftools
        org-noter
        org-noter-pdftools
        org-fragtog
        ;; FIXME: test latex preview problem
	      ;; (ox-latex :location built-in)
	      ;; (ox-beamer :location built-in)
	      ;; (ox-bibtex :location built-in)
	      ;; (ox-html :location built-in)
        ;; (ox-odt :location built-in)
        ;; ----
        ;; maxpix
        ;; equation to latex code generation, requires a paid appkey
        ;; - https://github.com/jethrokuan/mathpix.el
        ;; - https://accounts.mathpix.com/account
        ;; (maxthpix
        ;;  :location (recipe :fetcher github :repo "jethrokuan/mathpix.el"))
        ;; org-tanglesync ;; not very useful
        ;; polymode
        ;; (polybrain :location (recipe :fetcher github :repo "Kungsgeten/polybrain.el")) ;; not very useful
        ;; TODO: move to python(-extra) layer
        ;; conda ;; FIXME: should be in python layer too
        ;; anaconda-mode ;; FIXME: should be in python layer too
        ;; TODO: Add plantuml layer
        ;; plantuml-mode
        ;; flycheck-plantuml
        ;; TODO: Add graphviz layer
        ;; graphviz-dot-mode
        (org-roam-ui :requires org-roam)
        (org-roam-bibtex :requires org-roam)
        (org-fc :location (recipe :fetcher git :url "https://git.sr.ht/~l3kn/org-fc"
                                  :files (:defaults "awk" "demo.org")))
        org-web-tools
        ))

(defun org-extra/init-ox-beamer ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-beamer))
  (use-package ox-beamer
    :defer t
    :after (ox ox-latex)))


(defun org-extra/init-ox-bibtex ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-bibtex))
  (use-package ox-bibtex
    :defer t
    :after (ox ox-latex)
    ;;:ensure-system-package bibtex2html
    ))


(defun org-extra/init-ox-html ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-html))
  (use-package ox-html
    :defer t
    :after ox
    :init
    (progn
      ;; 更好的解决方法: html输出时换行带来的多余空格
      ;; REF: (@url :file-name "http://www.newsmth.net/nForum/#!article/Emacs/103680" :display "newsmth.net")
      ;; 这儿有一种临时解决方法[1][2]，通过给函数 org-html-paragraph 添加
      ;; advice，使得导出 html 前自动将段落中的多行中文合并为一行，且不会影响
      ;; 源文件，个人认为还算实用，可供参考：
      ;; REF: (@url :file-name "http://fasheng.github.io/blog/2013-09-25-fix-chinese-space-issue-when-exporting-org-mode-to-html.html" :display "[1]")
      ;; REF: (@url :file-name "https://gist.github.com/fasheng/6696398 " :display "[2]")
      ;; NOTE: add to `org-post-load'
      (defadvice org-html-paragraph (before fsh-org-html-paragraph-advice
                                            (paragraph contents info) activate)
        "Join consecutive Chinese lines into a single long line \
without unwanted space when exporting org-mode to html."
        (let ((fixed-contents)
              (orig-contents (ad-get-arg 1))
              (reg-han "[[:multibyte:]]"))
          (setq fixed-contents (replace-regexp-in-string
                                (concat "\\(" reg-han "\\) *\n *\\(" reg-han "\\)")
                                "\\1\\2" orig-contents))
          (ad-set-arg 1 fixed-contents)
          )))))

(defun org-extra/init-ox-odt ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-odt))
  (use-package ox-odt
    :defer t
    :after ox
    :init
    (progn
      (setq org-odt-data-dir (concat org-directory "/addon/odt/styles")))))


(defun org-extra/init-ox-latex()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-latex))
  (use-package ox-latex
    :defer t
    :after ox
    :config
    (progn
      (setq org-latex-coding-system 'utf-8-unix
            org-latex-table-caption-above nil
            org-latex-tables-column-borders t
            ;; code listing settings, new `minted' is also supported
            org-latex-listings t
            ;; org-latex-listings 'minted
            ;; FIXME: fix the bug of current version
            ;; org-latex-preview-ltxpng-directory "./"
            )

      ;; NOTE: Use org to write the draft of the document, and you can
      ;; fine-tuning of the latex template for the final version.
      (setq org-latex-classes
            '(("beamer" "\\documentclass[presentation,9pt]{beamer}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

              ("article" "\\documentclass[11pt]{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

              ("report" "\\documentclass[11pt]{report}"
               ;; ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

              ("book" "\\documentclass[11pt]{book}"
               ;; ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

              ("letter" "\\documentclass[11pt]{letter}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

              ("scrartcl" "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

              ("scrreprt" "\\documentclass{scrreprt}"
               ;; ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

              ("scrbook" "\\documentclass{scrbook}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

              ;; ("beamer" "\\documentclass{beamer}"
              ;;  org-beamer-sectioning)

              ("elegantnote" "\\documentclass{elegantnote}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

              ("elegantpaper" "\\documentclass{elegantpaper}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

              ("elegantbook" "\\documentclass{elegantbook}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ))

      ;; NOTE: The default `inputenc' and `fontenc' packages conflicts
      ;; with `xecjk' and `ctex'. The encoding of the input latex files
      ;; don't need to be set.
      (setq org-latex-default-packages-alist
            '(("" "fixltx2e" nil) ("" "graphicx" t) ("" "longtable" nil)
              ("" "float" nil) ("" "wrapfig" nil) ("" "rotating" nil)
              ("normalem" "ulem" t) ("" "amsmath" t) ("" "textcomp" t)
              ("" "marvosym" t) ("" "wasysym" t) ("" "amssymb" t)
              ("" "hyperref" nil) "\\tolerance=1000"
              ;;("" "amsmath" t) ;; this package cause error, no need
              ))

      ;; NOTE: Alist of packages to be inserted in every LaTeX header.
      ;; These will be inserted after `org-latex-default-packages-alist'.
      (setq org-latex-packages-alist
            '(;; The following 3 packages are required if using `listings'
              ;; ("svgnames, table" "xcolor" t)
              ("" "xcolor" t)
              ("" "listings" t)
              ;; ("" "minted" t)
              ("" "setspace" nil)
              ;; Display various latex-related logos
              ;; ("" "metalogo" t) ;; conflict with tipa package
              ;; ("" "mflogo" t) ("" "texnames" t) ;; not very useful
              ;; ("" "amsmath" nil) ;; this package cause error, no need
              ;; ("" "tikz" nil)
              ;; xelatex font adjustment (by default)
              ;; ("" "fontspec" nil)
              ;; Some extra text markups
              ;; ("normalem" "ulem" t)
              ;; Some figure-related packages
              ;; ("" "rotating" t) ("" "subfig" t)
              ;; Some table-related packages
              ;; ("" "booktabs" t) ("" "longtable" nil) ("" "multirow" t)
              ;; ("" "tabularx" t) ("" "warpcol" t)
              ;; Some document layout/structure-related packages
              ;; ("" "etex" nil) ("" "multicol" nil) ("" "multind" nil)
              ;; ("" "titlesec" nil)
              ))

      ;; NOTE: LaTeX header that will be used when processing a fragment
      (setq org-format-latex-header
            "\\documentclass{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\usepackage{tikz}
\\usetikzlibrary{
arrows, calc, fit, patterns, plotmarks, shapes, shadows,
datavisualization, er, automata, backgrounds, chains, topaths,
trees, matrix, fadings, shadings, through, positioning, scopes,
intersections, fixedpointarithmetic, petri,
decorations.pathreplacing, decorations.pathmorphing,
decorations.markings}
\\usepackage{pgfgantt}

\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

      (setq org-format-latex-options
            '(:foreground default
                          :background default
                          :scale 1.0
                          :html-foreground "Black"
                          :html-background "Transparent"
                          :html-scale 1.0
                          :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

      (setq org-format-latex-signal-error t)
      (setq org-latex-create-formula-image-program 'imagemagick)

      ;; Use latexmk instead of xelatex
      (setq org-latex-pdf-process
            '("latexmk -pdf -bibtex -f -silent %b"
              "latexmk -c"))
      )))

(defun org-extra/init-org-crypt ()
  (spacemacs|use-package-add-hook org :post-config (require 'org-crypt))
  (use-package org-crypt
    :defer t
    :after org
    :commands (org-crypt-use-before-save-magic)
    :config
    (progn
      ;; (org-crypt-use-before-save-magic)
      (setq org-use-tag-inheritance t   ;; Inherit tags in most of cases
            org-tags-exclude-from-inheritance (quote ("crypt" "prj" "book"))
            org-crypt-key nil)
      ;; (setq auto-save-default nil)
      ;; ;; Auto-saving does not cooperate with org-crypt.el: so you need
      ;; ;; to turn it off if you plan to use org-crypt.el quite often.
      ;; ;; Otherwise, you'll get an (annoying) message each time you
      ;; ;; start Org.
      ;; ;; To turn it off only locally, you can insert this:
      ;; ;;
      ;; ;; # -*- buffer-auto-save-file-name: nil; -*-
      )))


(defun org-extra/init-org-attach ()
  (spacemacs|use-package-add-hook org :post-config (require 'org-attach))
  (use-package org-attach
    :defer t
    :init
    (progn
      (setq org-link-abbrev-alist
            '(("att" . org-attach-expand-link))))))


;; load ob-ipython
(defun org-extra/init-ob-ipython ()
  (use-package ob-ipython
    :defer t
    :after ob))


;; load ob-async
(defun org-extra/init-ob-async ()
  (use-package ob-async
    :defer t
    :after ob
    :config
    (progn
      (setq ob-async-no-async-languages-alist '("ipython")))))


;; load org-pdftools
(defun org-extra/init-org-pdftools ()
  (use-package org-pdftools
    :hook (org-load . org-pdftools-setup-link)))


;; load org-noter
(defun org-extra/init-org-noter ()
  (use-package org-noter
    :defer t
    :config
    (add-hook 'org-noter-insert-heading-hook #'org-id-get-create)
    (setq org-noter-auto-save-last-location nil
          org-noter-always-create-frame nil)
    (defun org-brain-open-org-noter (entry)
      "Open `org-noter' on the ENTRY.
If run interactively, get ENTRY from context."
      (interactive (list (org-brain-entry-at-pt)))
      (org-with-point-at (org-brain-entry-marker entry)
        (org-noter)))))


;; load org-noter-pdftools
(defun org-extra/init-org-noter-pdftools ()
  (use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions
              #'org-noter-pdftools-jump-to-note))))


;; TEST: Moved to org layer config (.spacemacs)
;; load ob-plantuml
;; (defun org-extra/init-ob-plantuml ()
;;   (use-package ob-plantuml
;;     :defer t
;;     :after ob
;;     :config
;;     (setq org-plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\"")
;;           org-plantuml-jar-path (expand-file-name "/opt/plantuml/plantuml.jar"))
;;     ))

;; todo: move to python(-extra) layer
;; ;; load conda
;; (defun org/init-conda ()
;;   (use-package conda
;;     :defer t
;;     :after ob
;;     :config
;;     (progn
;;       (setq conda-anaconda-home "/opt/anaconda3/"
;;             conda-env-home-directory "~/.conda/"
;;             python-shell-virtualenv-root "~/.conda/envs")
;;       ;; (conda-env-initialize-interactive-shells)
;;       ;; (conda-env-autoactivate-mode t)
;;       ;; (conda-env-activate "py37_test") ;; not working
;;       )
;;     ;; :init
;;     ;; (progn
;;     ;;   ;; Fix org-capture json error
;;     ;; (conda-env-activate "py38_data")
;;     ;;   )
;;     ))

;; load org-roam-ui
(defun org-extra/init-org-roam-ui ()
  (use-package websocket
    :after org-roam)
  (use-package org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          ;; It is better to use an external browser
          ;; org-roam-ui-browser-function 'xwidget-webkit-browse-url
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)
    ;; (spacemacs|diminish org-roam-mode " Ⓡ" " R")
    (spacemacs|diminish org-roam-ui-mode " Ⓡ" " R")
    (spacemacs|diminish org-roam-ui-follow-mode)
    (spacemacs/set-leader-keys "aoru" 'org-roam-ui-mode)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "ru" 'org-roam-ui-mode
      "rw" 'org-roam-ui-follow-mode)
    ))

(defun org-extra/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam))

;; load mathpix, requires a paid account
;; (defun org-extra/init-mathpix ()
;;   (use-package mathpix
;;     :custom ((mathpix-app-id "app-id")
;;              (mathpix-app-key "app-key"))
;;     :bind
;;     ("C-x m" . mathpix-screenshot)
;;     ))

;; load org-fc
(defun org-extra/init-org-fc ()
  (use-package hydra)
  (use-package org-fc-hydra
    :config
    (setq org-fc-directories '("~/emacs/org/roam")))
  )

;; load org-fragtog
(defun org-extra/init-org-fragtog ()
  (use-package org-fragtog
    :init
    (add-hook 'org-mode-hook 'org-fragtog-mode)
    )
  )

;; load org-web-tools
(defun org-extra/init-org-web-tools ()
  (use-package org-web-tools
    :config
    (setq org-web-tools-archive-fn 'org-web-tools-archive--wget-tar
          org-web-tools-attach-archive-max-attempts 3
          org-web-tools-attach-archive-retry 10
          org-web-tools-attach-archive-retry-fallback nil)
    (setq org-web-tools-archive-wget-html-only-options
          '("--execute" "robots=off" "--adjust-extension" "--timestamping" "--no-directories"))
    (setq org-web-tools-archive-wget-options
          '("--ignore-tags=script,iframe" "--reject=eot,ttf,svg,otf,*.woff*" "--execute" "robots=off" "--adjust-extension" "--span-hosts" "--convert-links" "--page-requisites" "--timestamping" "--no-directories"))
    ;; keybindings
    (spacemacs/declare-prefix-for-mode 'org-mode "w" "web")
    (spacemacs/declare-prefix-for-mode 'org-mode "wi" "insert")
    (spacemacs/declare-prefix-for-mode 'org-mode "wa" "archive")
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "wr" 'org-web-tools-read-url-as-org
      "wc" 'org-web-tools-convert-links-to-page-entries
      "wil" 'org-web-tools-insert-link-for-url
      "wie" 'org-web-tools-insert-web-page-as-entry
      "waa" 'org-web-tools-archive-attach
      "wav" 'org-web-tools-archive-view
      )
    )
  )
;;; packages.el ends here
