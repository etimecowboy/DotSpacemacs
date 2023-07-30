;; (defun spacemacs//python-setup-eldoc ()
;;   "Setup anaconda eldoc."
;;   (eldoc-mode)
;;   ;; (anaconda-eldoc-mode)
;;   )

;; (defun spacemacs/anaconda-view-forward-and-push ()
;;   "Find next button and hit RET"
;;   (interactive)
;;   (forward-button 1)
;;   (call-interactively #'push-button))

(defun spacemacs//python-sort-imports ()
  ;; py-isort-before-save checks the major mode as well, however we can prevent
  ;; it from loading the package unnecessarily by doing our own check
  (when (and python-sort-imports-on-save
             (derived-mode-p 'python-mode))
    (py-isort-before-save)))
