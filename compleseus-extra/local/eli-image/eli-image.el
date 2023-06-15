;; REF: https://emacs-china.org/t/embark-hack/22205/2 by VagrantJoker

(require 'posframe)
(require 'embark)

;;; Custom Variables

(defgroup eli-image nil
  "Find image file with preview popup"
  :tag "eli-image"
  :group 'vertico-posframe)

(defcustom eli-image-default-directory "~/Downloads"
  "The default directory that you looks for an image file."
  :group 'eli-image
  :type 'string)

(defvar recover-vertico-posframe-mode-p nil)

(defun eli-image-preview (&rest _args)
  (let* ((target (embark--targets))
         (file-path (plist-get (car target) :target))
         (name (file-name-nondirectory file-path))
         (mode (assoc-default name auto-mode-alist #'string-match)))
    (posframe-hide-all)
    (when (memq mode '(image-mode))
      (with-current-buffer (get-buffer-create "*image*")
        (setq inhibit-read-only t)
        (erase-buffer)
        (insert-file-contents file-path)
        (set-auto-mode-0 mode))
      (when (posframe-workable-p)
        (posframe-show "*image*"
                       :poshandler #'posframe-poshandler-window-center)))))

;;;###autoload
(defun eli-select-image ()
  (interactive)
  (let ((default-directory eli-image-default-directory))
    (call-interactively 'find-file)))

(advice-add 'eli-select-image
            :before (lambda ()
                      (if (featurep 'vertico-posframe)
                          (when vertico-posframe-mode
                            (vertico-posframe-mode -1)
                            (setq recover-vertico-posframe-mode-p t)))
                      (add-hook 'post-command-hook #'eli-image-preview)))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (remove-hook 'post-command-hook #'eli-image-preview)
            (posframe-delete-all)
            (if (featurep 'vertico-posframe)
                (when recover-vertico-posframe-mode-p
                  (vertico-posframe-mode 1)
                  (setq recover-vertico-posframe-mode-p nil)
                  ))))

(provide 'eli-image)
