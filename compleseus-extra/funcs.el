;;; funcs.el --- Compleseus-extra Layer functions File for Spacemacs
;; Time-stamp: <2023-01-08 Sun 03:00 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; REF: https://github.com/oantolin/embark/blob/master/README.org
(defun embark-act-noquit ()
  "Run action but don't quit the minibuffer afterwards."
  (interactive)
  (let ((embark-quit-after-action nil))
    (embark-act)))

(defun embark-act-quit ()
  "Run action but don't quit the minibuffer afterwards."
  (interactive)
  (let ((embark-quit-after-action t))
    (embark-act)))

;; 拼音搜索
;; REF:
;; 1. https://emacs-china.org/t/straight-ivy-helm-selectrum/11523/80
;; 2. https://emacs-china.org/t/vertico/17913/2
;; FIXME: not working from 2023-01-08
;; (defun eh-orderless-regexp (orig_func component)
;;   (let ((result (funcall orig_func component)))
;;     (pyim-cregexp-build result)))

;; (advice-add 'orderless-regexp :around #'eh-orderless-regexp)

;; consult-org / consult-org-roam already have these functions.
;; ;; REF: https://emacs-china.org/t/embark-hack/22205
;; ;; 1. consult-org-heading
;; (defun consult-heading-insert-backlink (target)
;;   (let* ((marker (plist-get
;;                   (text-properties-at 0 target)
;;                   'consult--candidate))
;;          (headline-name (substring (org-no-properties target)
;;                                    0 -1))
;;          (headline-id (save-excursion
;;                         (with-current-buffer
;;                             (marker-buffer marker)
;;                           (goto-char marker)
;;                           (org-id-get-create)))))
;;     (org-insert-link
;; 	   nil (concat "id:" headline-id) headline-name)))

;; ;; (embark-define-keymap embark-org-heading-map
;; ;;   "Keymap for Embark heading actions."
;; ;;   ("i" embark-insert)
;; ;;   ("b" consult-heading-insert-backlink)
;; ;;   ("w" embark-copy-as-kill)
;; ;;   ("q" embark-toggle-quit)
;; ;;   ("E" embark-export)
;; ;;   ("S" embark-collect)
;; ;;   ("L" embark-live)
;; ;;   ("B" embark-become)
;; ;;   ("A" embark-act-all)
;; ;;   ("C-s" embark-isearch)
;; ;;   ("SPC" mark)
;; ;;   ("DEL" delete-region))

;; ;; move to config.el
;; ;; (add-to-list 'embark-keymap-alist '(consult-org-heading . embark-org-heading-map))

;; ;; 2. org-roam support

;; (defun org-roam-backlinks-query* (NODE)
;;   "Gets the backlinks of NODE with `org-roam-db-query'."
;;   (org-roam-db-query
;;    [:select [source dest]
;; 		    :from links
;; 		    :where (= dest $s1)
;; 		    :and (= type "id")]
;;    (org-roam-node-id NODE)))

;; (defun org-roam-backlinks-p (SOURCE NODE)
;;   "Predicate function that checks if NODE is a backlink of SOURCE."
;;   (let* ((source-id (org-roam-node-id SOURCE))
;; 	     (backlinks (org-roam-backlinks-query* SOURCE))
;; 	     (id (org-roam-node-id NODE))
;; 	     (id-list (list id source-id)))
;;     (member id-list backlinks)))

;; (defun org-roam-backlinks--read-node-backlinks (source)
;;   "Runs `org-roam-node-read' on the backlinks of SOURCE.
;;  The predicate used as `org-roam-node-read''s filter-fn is
;;  `org-roam-backlinks-p'."
;;   (org-roam-node-read nil (apply-partially #'org-roam-backlinks-p source)))

;; (defun org-roam-backlinks-node-read (entry)
;;   "Read a NODE and run `org-roam-backlinks--read-node-backlinks'."
;;   (let* ((node (get-text-property 0 'node entry))
;;          (backlink (org-roam-backlinks--read-node-backlinks node)))
;;     (find-file (org-roam-node-file backlink))))

;; ;; (embark-define-keymap embark-org-roam-map
;; ;;   "Keymap for Embark org roam actions."
;; ;;   ("i" org-roam-node-insert)
;; ;;   ("s" embark-collect)
;; ;;   ("b" eli-org-roam-backlinks-node-read))

;; ;; move to config.el
;; ;; (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map))
