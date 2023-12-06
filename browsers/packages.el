;;; packages.el --- browsers layer packages File for Spacemacs
;; Time-stamp: <2023-12-06 Wed 14:23 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;
;;; Code:

(defconst browsers-packages
  '(;; eaf ;; belong to eaf layer
    eww ;; belong to eww layer
    w3m
    link-hint ;; belong to spacemacs-editing layer
    org
    browse-url
    ;; ace-link ;; replaced by link-hint
    ))

;; (defun browsers/post-init-eaf ()
;;   (add-list-to-list 'eaf-browser-keybinding
;;                     '(("C-c B" . "xy/eaf-browser-browse-with-brave")
;;                       ("C-c C" . "xy/eaf-browser-browse-with-chrome")
;;                       ("C-c E" . "xy/eaf-browser-browse-with-eww")
;;                       ("C-c Y" . "xy/eaf-browser-browse-with-lynx")
;;                       ("C-c L" . "xy/eaf-browser-browse-with-elinks")
;;                       ("C-c W" . "xy/eaf-browser-browse-with-w3m"))
;;                     ))

(defun browsers/post-init-eww ()
  (setq shr-max-image-proportion 0.3
        shr-use-fonts t
        shr-max-width 90)
  (setq eww-retrieve-command '("google-chrome" "--headless" "--dump-dom"))
  ;; (define-key eww-link-keymap (kbd "C-c D") 'xy/eww-browse-with-eaf-browser)
  ;; (define-key eww-mode-map (kbd "C-c D") 'xy/eww-browse-with-eaf-browser)
  (define-key eww-link-keymap (kbd "C-c B") 'xy/eww-browse-with-brave)
  (define-key eww-mode-map (kbd "C-c B") 'xy/eww-browse-with-brave)
  (define-key eww-link-keymap (kbd "C-c C") 'xy/eww-browse-with-chrome)
  (define-key eww-mode-map (kbd "C-c C") 'xy/eww-browse-with-chrome)
  (define-key eww-link-keymap (kbd "C-c Y") 'xy/eww-browse-with-lynx)
  (define-key eww-mode-map (kbd "C-c Y") 'xy/eww-browse-with-lynx)
  (define-key eww-link-keymap (kbd "C-c E") 'xy/eww-browse-with-elinks)
  (define-key eww-mode-map (kbd "C-c E") 'xy/eww-browse-with-elinks)
  (define-key eww-link-keymap (kbd "C-c W") 'xy/eww-browse-with-w3m)
  (define-key eww-mode-map (kbd "C-c W") 'xy/eww-browse-with-w3m)
  (define-key eww-link-keymap "o" 'link-hint-open-link)
  (define-key eww-link-keymap "O" 'link-hint-copy-link)
  (define-key eww-mode-map "o" 'link-hint-open-link)
  (define-key eww-mode-map "O" 'link-hint-copy-link)
  )

(defun browsers/init-w3m ()
  (use-package w3m
    :defer t
    :bind (:map w3m-mode-map
                ;; ("C-c D" . xy/w3m-browse-with-eaf-browser)
                ("C-c B" . xy/w3m-browse-with-brave)
                ("C-c E" . xy/w3m-browse-with-eww)
                ("C-c C" . xy/w3m-browse-with-chrome)
                ("o"     . link-hint-open-link)
                ("O"     . link-hint-copy-link)
                )
    ;; :init
    ;; (setq-default w3m-resize-image-scale 25)
    :config
    (setq w3m-bookmark-file-coding-system 'utf-8-unix
          ;; w3m-default-coding-system   'utf-8-unix
          ;; w3m-coding-system           'utf-8-unix
          ;; w3m-input-coding-system     'utf-8-unix
          ;; w3m-output-coding-system    'utf-8-unix
          ;; w3m-file-coding-system      'utf-8-unix
          ;; w3m-file-name-coding-system 'utf-8-unix
          ;; w3m-terminal-coding-system  'utf-8-unix
          w3m-default-save-directory "~/下载/w3m/"
          w3m-confirm-leaving-secure-page nil
          w3m-cookie-accept-bad-cookies 'ask
          w3m-default-display-inline-images t
          w3m-add-tab-number t
          w3m-fill-column 90
          w3m-resize-images t
          w3m-resize-image-scale 50
          w3m-new-session-in-background t
          ;; w3m-command-arguments '("-cookie" "-F"))
          ;; w3m-favicon-use-cache-file t
          ;; w3m-keep-cache-size 500
          ;; w3m-new-session-url "about:blank"
          ;; w3m-prefer-cache t
          ;; w3m-use-cookies t
          ;; w3m-use-ange-ftp t
          ;; w3m-use-favicon nil
          ;; w3m-use-mule-ucs t
          ;; w3m-view-this-url-new-session-in-background t
          )
    ))

(defun browsers/post-init-link-hint ()
  (setq link-hint-avy-style 'at-full)
  (setq link-hint-action-fallback-commands
        (list :open
              (lambda () (condition-case _
                             (progn
                               (embark-dwim)
                               t)
                           (error nil)))))

  ;; add keys to major modes of standard packages
  (with-eval-after-load 'info
    (define-key Info-mode-map "o" 'link-hint-open-link)
    (define-key Info-mode-map "O" 'link-hint-copy-link))

  (with-eval-after-load 'help-mode
    (define-key help-mode-map "o" 'link-hint-open-link)
    (define-key help-mode-map "O" 'link-hint-copy-link))

  (with-eval-after-load 'woman
    (define-key woman-mode-map "o" 'link-hint-open-link)
    (define-key woman-mode-map "O" 'link-hint-copy-link))
  )


(defun browsers/post-init-org ()
  (setq org-speed-commands
        (cons '("o" . link-hint-open-link) org-speed-commands))
  (setq org-speed-commands
        (cons '("O" . link-hint-save-link) org-speed-commands))
  )

(defun browsers/init-browse-url ()
  (use-package browse-url
    :defer t
    :config
    ;; override this function for my need
    (defun browse-url-default-browser (url &rest args)
      "Find a suitable browser and ask it to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window, if possible, otherwise use
a random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument ARGS is used
instead of `browse-url-new-window-flag'."
      (apply
       (cond
        ((memq system-type '(windows-nt ms-dos cygwin))
         'browse-url-default-windows-browser)
        ((memq system-type '(darwin))
         'browse-url-default-macosx-browser)
        ((featurep 'haiku)
         'browse-url-default-haiku-browser)
        ;; ((featurep 'eaf-browser) 'eaf-open-browser)
        ((browse-url-can-use-xdg-open) 'browse-url-xdg-open)
        ((executable-find browse-url-firefox-program) 'browse-url-firefox)
        ((executable-find browse-url-chromium-program) 'browse-url-chromium)
        ((executable-find browse-url-kde-program) 'browse-url-kde)
        ((executable-find browse-url-chrome-program) 'browse-url-chrome)
        ((executable-find browse-url-webpositive-program) 'browse-url-webpositive)
        ((executable-find browse-url-xterm-program) 'browse-url-text-xterm)
        (t #'eww-browse-url))
       url args))

    (function-put 'browse-url-default-browser 'browse-url-browser-kind
                  ;; Well, most probably external if we ignore EWW.
                  'external)
    ))

;; (defun browsers/pre-init-ace-link ()
;;   (spacemacs|use-package-add-hook ace-link
;;     :pre-init
;;     ;; j or o to open links in ace style
;;     (spacemacs/set-leader-keys-for-major-mode 'org-mode
;;       "jj" 'ace-link-org
;;       "jo" 'ace-link-org)
;;     (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
;;       "j" 'ace-link-org-agenda
;;       "o" 'ace-link-org-agenda)
;;     (spacemacs/set-leader-keys-for-major-mode 'Info-mode
;;       "j" 'ace-link-info)
;;     (spacemacs/set-leader-keys-for-major-mode 'Custom-mode
;;       "j" 'ace-link-custom
;;       "o" 'ace-link-custom)
;;     (spacemacs/set-leader-keys-for-major-mode 'compilation-mode
;;       "j" 'ace-link-compilation
;;       "o" 'ace-link-compilation)
;;     (spacemacs/set-leader-keys-for-major-mode 'xref--xref-buffer-mode
;;       "j" 'ace-link-xref
;;       "o" 'ace-link-xref)

;;     ;; not working
;;     ;; (spacemacs/set-leader-keys-for-major-mode 'Man-mode
;;     ;;   "j" 'ace-link-man)
;;     ;; (spacemacs/set-leader-keys-for-major-mode 'help-mode
;;     ;;   "j" 'ace-link-help)
;;     ))
