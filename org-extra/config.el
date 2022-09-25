;;; config.el --- Org-extra configuration File for Spacemacs
;; Time-stamp: <2022-09-25 Sun 02:32 by xin on tufg>
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

(ad-activate 'org-babel-execute-src-block)

(add-hook 'org-babel-after-execute-hook #'xy/org-babel-after-execute)
;; (add-hook 'after-save-hook #'org-redisplay-inline-images)
(add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list)

;; FIXME: try to solve cannot complete org-roam nodes
;; (add-hook 'org-mode-hook #'org-roam-update-org-id-locations) ;; too slow
;; (add-hook 'org-mode-hook #'org-roam-node-read--completions)
;; (add-hook 'org-mode-hook #'org-roam-buffer-refresh)
;; FIXME: Reload local settings when org file headings changed
;; (add-hook 'after-save-hook #'org-mode-restart)

;; NOTE: this cause multiple times of loading lob.
;; We only want to load it once.
;; (add-hook 'org-mode-hook #'xy/load-lob)
(with-eval-after-load "org-roam"
  (xy/load-lob))
