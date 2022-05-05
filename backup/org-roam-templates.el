  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target (file+head
                                             "%<%Y%m%d%H%M%S>-${slug}.org"
                                             "#+title: ${title}
")
           :unnarrowed t :empty-lines 1 :prepend t)

          ("p" "project" plain "%?" :target (file+head
                                             "%<%Y%m%d%H%M%S>-${slug}.org"
                                             "#+title: ${title}
#+category: ${title}
#+filetags: PROJECT
* Goal

* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:

** TODO Define the goal of the project.
** TODO Add areas-of-responsibility tags.
** TODO Set a project deadline.
") :unnarrowed t :empty-lines 1 :prepend t)))

  ;; NOTE: it seems argument switches doesn't work in org-roam-capture-ref-templates
;;   (setq org-roam-capture-ref-templates
;;         '(;; TODO: automatically download the original webpage as a compressed
;;           ;;       attachment using `org-web-tools-archieve'
;;           ("r" "ref" plain "%?" :target (file+head
;;                                          "${slug}.org"
;;                                          "#+title: ${title}
;; #+filetags: REF
;; * Local backup
;; :PROPERTIES:
;; :ROAM_EXCLUDE: t
;; :END:

;; * Abstract
;; ") :unnarrowed t :empty-lines 1 :jump-to-captured t)
;;             ;;
;;             ;; REF: https://www.zmonster.me/2020/06/27/org-roam-introduction.html
;;             ;; TODO:
;;             ;; 1. [X] creating an annotation headline in the existing note file
;;             ;; 2. [X] creating an annatation headline in a new node file
;;             ;; 3. [ ] same as 1&2 but under the ``Annotations'' headline.
;;             ;;
;;           ("a" "annote" plain "* %U %?
;; ${body}
;; " :target (file+head "${slug}.org"
;;                      "#+title: ${title}
;; #+filetags: REF
;; * Local backup
;; :PROPERTIES:
;; :ROAM_EXCLUDE: t
;; :END:

;; * Abstract
;; ") :immediate-finish t :empty-lines 1 :jump-to-captured t)))

(setq org-roam-capture-ref-templates
      ;; REF: https://www.zmonster.me/2020/06/27/org-roam-introduction.html
      '(;; TODO: automatically download the original webpage as a compressed
        ;;       attachment using `org-web-tools-archieve'
        ;;
        ;; TODO: insert the annotation under the ``Annotations'' headline.
        ("a" "annote" entry "** %?
:LOGBOOK:
- Create time: %U
:END:

${body}" :target (file+head
                  "${slug}.org"
                  "#+title: ${title}
#+filetags: ref
* Abstract

* Local backup
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:

") :immediate-finish t :empty-lines 1 :jump-to-captured t)))

;; NOTE:
;; 1. Both new and old templates are working `org-roam-capture-ref-templates'
;; 2. Neither can perfectly insert new annotations under the /Annotations/
;;    headline like `org-capture-templates'

;; Old style template, it is working without the new :target argument
;; REF: https://github.com/org-roam/org-roam/issues/1845
;; (setq org-roam-capture-ref-templates
;;
;;           ("a" "annote" entry "** %?
;; :LOGBOOK:
;; - Create time: %U
;; - From: %a
;; :END:

;; ${body}" :if-new (file+head+olp "${slug}.org"
;;                                 "#+title: ${title}
;; #+filetags: REF
;; * Local backup
;; :PROPERTIES:
;; :ROAM_EXCLUDE: t
;; :END:


;; * Abstract


;; * Annotations
;; " ("Annotations")) :immediate-finish t :empty-lines 1 :jump-to-captured t)))

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %U %?" :target (file+head
                                                  "%<%Y-%m-%d>.org"
                                                  "#+title: %<%Y-%m-%d>
* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:
") :unnarrowed t :empty-lines 1 :prepend t)))
