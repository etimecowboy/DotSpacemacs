;; REF: https://emacs-china.org/t/embark-hack/22205/2 by VagrantJoker
;; FIXME: improve and integrates in my workflow
;; workflow that make it work:
;; 1. in org-mode, call find-file
;; 2. call eli-select-images in minibuffer
;; 3. C-g to quit back to find-file
;; 4. browse to the image folder in minibuffer
;; 5. image preview works
(require 'posframe)
(require 'embark)
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

(defun eli-select-images ()
  (interactive)
  (let ((default-directory "~/org/roam/data/"))
    (call-interactively 'find-file)))

(advice-add 'eli-select-images
            :before (lambda ()
                      (add-hook 'post-command-hook #'eli-image-preview)))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (remove-hook 'post-command-hook #'eli-image-preview)
            (posframe-delete-all)))

(provide 'eli-image)
