;; REF: https://emacs-china.org/t/embark-hack/22205/2 by VagrantJoker

(require 'posframe)
(require 'embark)

;;; Custom Variables

(defgroup eli-image nil
  "Find image file with preview popup"
  :tag "eli-image"
  :group 'vertico-posframe)

(defcustom eli-image-default-directory "~/Pictures"
  "The default directory that you looks for an image file."
  :group 'eli-image
  :type 'string)

(defvar eli-image-recover-vertico-posframe-mode-p nil)

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
                       :poshandler #'posframe-poshandler-frame-center)))))

;; REF: https://www.emacswiki.org/emacs/InsertFileName
(defun get-file-path (filename &optional args)
  "Interactively return name of file FILENAME after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  (interactive "*fInsert file path: \nP")
  (let ((default-directory eli-image-default-directory))
    (cond ((eq '- args)
           (file-relative-name filename))
          ((not (null args))
           (expand-file-name filename))
          (t
           filename))))

;;;###autoload
(defun eli-image-find-file ()
  (interactive)
  (let ((default-directory eli-image-default-directory))
    (call-interactively 'find-file)))

;;;###autoload
(defun eli-image-insert-path ()
  (interactive)
  (let ((default-directory eli-image-default-directory))
    (insert (call-interactively 'get-file-path))))

;;;###autoload
(defun eli-image-org-attach ()
  (interactive)
  (if (featurep 'org-attach)
      (org-attach-attach
       (let ((default-directory eli-image-default-directory))
         (call-interactively 'get-file-path)) nil 'cp)
    (message "Must call in org-mode with `org-attach.el' loaded.")))


(advice-add 'eli-image-find-file
            :before (lambda ()
                      (if (featurep 'vertico-posframe)
                          (when vertico-posframe-mode
                            (vertico-posframe-mode -1)
                            (setq eli-image-recover-vertico-posframe-mode-p t)))
                      (add-hook 'post-command-hook #'eli-image-preview)))

(advice-add 'eli-image-insert-path
            :before (lambda ()
                      (if (featurep 'vertico-posframe)
                          (when vertico-posframe-mode
                            (vertico-posframe-mode -1)
                            (setq eli-image-recover-vertico-posframe-mode-p t)))
                      (add-hook 'post-command-hook #'eli-image-preview)))

(advice-add 'eli-image-org-attach
            :before (lambda ()
                      (if (featurep 'vertico-posframe)
                          (when vertico-posframe-mode
                            (vertico-posframe-mode -1)
                            (setq eli-image-recover-vertico-posframe-mode-p t)))
                      (add-hook 'post-command-hook #'eli-image-preview)))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (remove-hook 'post-command-hook #'eli-image-preview)
            (posframe-delete-all)
            (if (featurep 'vertico-posframe)
                (when eli-image-recover-vertico-posframe-mode-p
                  (vertico-posframe-mode 1)
                  (setq eli-image-recover-vertico-posframe-mode-p nil)
                  ))))

(provide 'eli-image)
