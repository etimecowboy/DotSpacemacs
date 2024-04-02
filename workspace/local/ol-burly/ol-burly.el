;;; ol-burly.el - Support for burly links in Org mode
(require 'ol)

(org-link-set-parameters "burly"
                         :follow #'org-burly-open
                         ;; :export #'org-burly-windows-export
                         ;;:store #'org-burly-windows-store-link
                         )

(defun org-burly-open (path _)
  "Visit the  on PATH.
PATH should be a topic that can be thrown at the man command."
  (funcall burly-open-url path))

;; (defun org-blu-store-link ()
;;   "Store a link to a man page."
;;   (when (memq major-mode '(Man-mode woman-mode))
;;     ;; This is a man page, we do make this link.
;;     (let* ((page (org-man-get-page-name))
;;            (link (concat "man:" page))
;;            (description (format "Man page for %s" page)))
;;       (org-link-store-props
;;        :type "man"
;;        :link link
;;        :description description))))

;; (defun org-man-get-page-name ()
;;   "Extract the page name from the buffer name."
;;   ;; This works for both `Man-mode' and `woman-mode'.
;;   (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
;;       (match-string 1 (buffer-name))
;;     (error "Cannot create link to this man page")))

;; (defun org-man-export (link description format _)
;;   "Export a man page link from Org files."
;;   (let ((path (format "http://man.he.net/?topic=%s&section=all" link))
;;         (desc (or description link)))
;;     (pcase format
;;       (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
;;       (`latex (format "\\href{%s}{%s}" path desc))
;;       (`texinfo (format "@uref{%s,%s}" path desc))
;;       (`ascii (format "%s (%s)" desc path))
;;       (t path))))

(provide ol-burly)
;;; ol-burly.el ends here
