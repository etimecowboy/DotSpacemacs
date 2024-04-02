;;; packages.el --- UI layer packages File for Spacemacs
;; Time-stamp: <2024-04-02 Tue 08:22 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; The official themes-megapack is too big. This layer just add selected themes.
;; Top emacs themes can be found in https://emacsthemes.com/popular/index.html
;;
;;; Code:

(defconst ui-packages
  '(
    doom-themes
    github-dark-vscode-theme
    popwin
    holy-mode ;; belongs to spacemacs-boostrap layer
    hybrid-mode ;; belongs to spacemacs-bootstrap layer
    which-key ;; belongs to spacemacs-bootstrap layer
    persistent-scratch ;; belongs to spacemacs-editing layer
    iscroll
    spacious-padding
    breadcrumb
    writeroom-mode
    tab-bar
    tab-line
    visual-fill-column
    adaptive-wrap
    ;; mini-header-line
    ;; path-headerline-mode
    ;; minibuffer-header
    ;; (elegant :location (recipe :fetcher github :repo "rougier/elegant-emacs"))
    ;; (nano :location (recipe :fetcher github :repo "rougier/nano-emacs"))
    ;; emacs-everywhere ;; FIXME: It does not work in Wayland.
    ;; god-mode
    ;; color-theme-sanityinc-tomorrow
    ;; zenburn-theme
    ;; (dockwin :location (recipe :fetcher github :repo "pronobis/dockwin")) ;; too old
    ;; per-buffer-theme ;; overridden by spacemacs-theme
    ))

(defun ui/init-doom-themes ()
  (use-package doom-themes
    ;; :ensure t
    :defer t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; (load-theme 'doom-one t)
    ;; Enable flashing mode-line on errors
    ;; (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    ;; (doom-themes-neotree-config)
    ;; or for treemacs users
    ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    ;; (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    ;; (doom-themes-org-config)
    ))

(defun ui/init-github-dark-vscode-theme ()
  (use-package github-dark-vscode-theme
    ))

(defun ui/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :pre-init
    (setq-default popwin:popup-window-width 40
                  popwin:popup-window-height 15)
    :post-config
    (setq popwin:adjust-other-windows t
          popwin:popup-window-position 'left
          popwin:popup-window-width 40
          popwin:popup-window-height 15
          popwin:reuse-window nil)

    ;; (defun popwin-restore-window-layout ()
    ;;   (winner-redo)
    ;;   (winner-redo))
    ;; (advice-add 'popwin-restore-window-layout
    ;;             :after 'popwin:close-popup-window)

    (define-key popwin:keymap (kbd "L") #'popwin:display-last-buffer)
    (define-key popwin:keymap (kbd "t") #'popwin:popup-buffer-tail)
    (define-key popwin:keymap (kbd "T") #'popwin:find-file-tail)
    (define-key popwin:keymap (kbd "C-g") #'popwin:close-popup-window)
    (define-key popwin:keymap (kbd "k") #'popwin:close-popup-window)
    (define-key popwin:keymap (kbd "q") #'popwin:close-popup-window)

    ;; (popwin-mode 1) ;; already enabled by spacemacs-visual layer
    ))

(defun ui/post-init-holy-mode ()
  (spacemacs|diminish holy-mode))

(defun ui/post-init-hybrid-mode ()
  (spacemacs|diminish hybrid-mode))

(defun ui/post-init-which-key ()
  (spacemacs|diminish which-key-mode))

(defun ui/post-init-persistent-scratch ()
  (spacemacs|diminish persistent-scratch-mode))

(defun ui/init-iscroll ()
  (use-package iscroll
    :defer t
    :hook ((dired-mode
            image-mode
            org-mode
            markdown-mode
            eww-mode
            w3m-mode
            doc-view-mode
            pdf-view-mode
            ) . iscroll-mode)
    :init
    (spacemacs|diminish iscroll-mode)
    ))

(defun ui/init-spacious-padding ()
  (use-package spacious-padding
    :defer t
    :custom
    (spacious-padding-widths '(:internal-border-width 10
                               :header-line-width 5
                               :mode-line-width 5
                               :tab-width 4
                               :right-divider-width 10
                               :scroll-bar-width 8))
    ;; :init
    ;; (spacemacs|diminish specious-padding-mode)
    ))


(defun ui/init-breadcrumb ()
  (use-package breadcrumb
    :ensure t
    :custom
    (breadcrumb-imenu-max-length 0.5)
    (breadcrumb-project-max-length 0.5)
    (breadcrumb-imenu-crumb-separator "⮞")
    (breadcrumb-project-crumb-separator "/")
    :config

    ;; NOTE: show header-line instead of mode-line, this makes tmux with status
    ;; bar book better
    ;; (breadcrumb-mode 1)

    ;; FIXME: “ (spacemacs/toggle-mode-line-off)”does not work
    ;; (modeline-mode -1)
    ;; (hidden-mode-line-mode 1)

    (custom-set-faces
     '(breadcrumb-face
       ((t (:extend t :background "dark green" :foreground "white" :height 0.7
                    :slant italic :weight light :underline t :overline t
                    :family "Consolas"
                    ;; :family "TerminusTTF"
                    ;; :family "Courier New"
                    ;; :family "Monospaced"
                    ;; :family "FiraCode Nerd Font Mono"
                    ))))
     '(breadcrumb-project-leaf-face
       ((t (:inherit (breadcrumb-project-crumbs-face
                      mode-line-buffer-id))))))
    ))

;; REF: https://github.com/ksjogo/mini-header-line/blob/master/mini-header-line.el
;; (defun ui/init-mini-header-line ()
;;   (use-package mini-header-line
;;     :ensure t
;;     :config
;;     (mini-header-line-mode 1)))

;; REF: http://emacs.rubikitch.com/path-headerline-mode/
;; (defun ui/init-path-headerline-mode ()
;;   (use-package path-headerline-mode
;;     :defer t
;;     :commands (path-header-line-on
;;                path-header-line-off
;;                path-headerline-mode)
;;     ;; :ensure t
;;     ;; :config
;;     ;; (path-headerline-mode +1)
;;     ))

;; REF: https://github.com/rougier/minibuffer-header
;; (defun ui/init-minibuffer-header ()
;;   (use-package minibuffer-header
;;     :ensure t
;;     :config
;;     (minibuffer-header-mode 1)))

;; (defun ui/init-elegant ()
;;   (use-package elegant))

;; (defun ui/init-nano ()
;;   (use-package nano
;;     :config
;;     (setq nano-font-family-monospaced "Sarasa Mono SC Nerd Font"
;;           nano-font-family-proportional nil
;;           nano-font-size 16)
;;     ))

;; (defun ui/init-emacs-everywhere ()
;;   (use-package emacs-everywhere
;;     :ensure t
;;     ))

;; (defun ui/init-god-mode ()
;;   (use-package god-mode
;;     :defer t
;;     :bind (("<escape>" . god-mode-all)) ;; optional: switch <escape> and <capslock>
;;     :init
;;     (setq god-exempt-major-modes nil
;;           god-exempt-predicates nil
;;           god-mode-enable-function-key-translation nil
;;           god-mode-alist '((nil . "C-")
;;                            ("g" . "M-")
;;                            ("G" . "C-M-")))
;;     ;;(spacemacs|diminish god-mode " ✝" " God") ;; not
;;     :config
;;     (when (featurep 'which-key)
;;       (which-key-enable-god-mode-support))
;;     ;;(spacemacs|diminish god-mode " ✝" " God")

;;     (custom-set-faces '(god-mode-lighter
;;                         ((t
;;                           (:background "orange red"
;;                                        :foreground "forest green"
;;                                        :weight extra-bold)))))

;;     (defun xy/toggle-cursor-display ()
;;       (if (or god-local-mode buffer-read-only)
;;           (progn
;;             (setq cursor-type 'box)
;;             (setq blink-cursor-interval 0.3)
;;             (blink-cursor-mode 1))
;;         (progn
;;           (setq cursor-type 'bar)
;;           (blink-cursor-mode -1))))

;;     (add-hook 'post-command-hook #'xy/toggle-cursor-display)
;;     ))

;; (defun ui/init-color-theme-sanityinc-tomorrow ()
;;   (use-package color-theme-sanityinc-tomorrow
;;     :defer t
;;     ))

;; (defun ui/init-zenburn-theme ()
;;   (use-package zenburn-theme
;;     :defer t
;;     ))

;; (defun ui/init-dockwin ()
;;   (use-package dockwin
;;     :defer t
;;     ))

;; (defun ui/init-per-buffer-theme ()
;;   (use-package per-buffer-theme
;;     :ensure t
;;     :config
;;     (setq per-buffer-theme/use-timer t)
;;     (setq per-buffer-theme/timer-idle-delay 0.1)
;;     ;; (setq per-buffer-theme/default-theme 'notheme)
;;     (setq per-buffer-theme/themes-alist
;;           '(
;;             ;; ((:theme . dichromacy)
;;             ;;  (:buffernames nil)
;;             ;;  (:modes
;;             ;;   haskell-mode haskell-interactive-mode))
;;             ((:theme . zenburn)
;;              (:buffernames nil)
;;              (:modes vterm-mode ansi-term-mode term-mode eshell-mode))
;;             ))
;;     ))


(defun ui/pre-init-writeroom-mode ()
  (spacemacs|use-package-add-hook writeroom-mode
    :post-init
    (setq writeroom-width 110
          writeroom-extra-line-spacing 0.25
          writeroom-global-effects '(;; writeroom-set-fullscreen
                                     writeroom-set-alpha
                                     writeroom-set-menu-bar-lines
                                     writeroom-set-tool-bar-lines
                                     writeroom-set-vertical-scroll-bars
                                     writeroom-set-bottom-divider-width
                                     writeroom-set-internal-border-width)
          ;; writeroom-header-line t
          writeroom-bottom-divider-width 1
          writeroom-restore-window-config t)
    ))


(defun ui/init-tab-bar ()
  (use-package tab-bar
    :custom
    ((tab-bar-show 1)
     (tab-bar-tab-hints t)
     (tab-bar-mode -1))
    :config
    ;; override tab buttons
    (setq tab-bar-new-button (propertize (if (char-displayable-p ?＋) " ＋ " " + "))
          tab-bar-close-button (propertize (if (char-displayable-p ?×) " × " " x ")
                                           'close-tab t
                                           :help "Click to close tab")
          tab-bar-back-button (propertize (if (char-displayable-p ?◀) " ◀ " " < "))
          tab-bar-forward-button (propertize (if (char-displayable-p ?▶) " ▶ " " > "))
          )
     ))

(defun ui/init-tab-line ()
  (use-package tab-line
    :init
    (defcustom tab-line-tab-min-width 15
      "Minimum width of a tab in characters."
      :type 'integer
      :group 'tab-line)

    (defcustom tab-line-tab-max-width 30
      "Maximum width of a tab in characters."
      :type 'integer
      :group 'tab-line)

    :custom
    (tab-line-tab-name-truncated-max 25)
    (tab-line-close-button-show t)
    (tab-line-close-tab-function #'xy--tab-line-close-tab)
    (tab-line-new-button-show t)
    (tab-line-separator " ")
    (tab-line-tab-name-function #'xy--tab-line-name-buffer)
    ;; make sure the `:config' part runs, and the first window tab correctly drawn
    (global-tab-line-mode -1)

    :config
    ;; override tab buttons
    (setq tab-line-right-button
          (propertize (if (char-displayable-p ?▶) " ▶ " " > ")
                      'keymap tab-line-right-map
                      'mouse-face 'tab-line-highlight
                      'help-echo "Click to scroll right")
          tab-line-left-button
          (propertize (if (char-displayable-p ?◀) " ◀ " " < ")
                      'keymap tab-line-left-map
                      'mouse-face 'tab-line-highlight
                      'help-echo "Click to scroll left")
          tab-line-close-button
          (propertize (if (char-displayable-p ?×) " × " " x ")
                      'keymap tab-line-tab-close-map
                      'mouse-face 'tab-line-close-highlight
                      'help-echo "Click to close tab")
          tab-line-new-button
          (propertize (if (char-displayable-p ?＋) " ＋ " " + ")
                      'keymap tab-line-add-map
                      'mouse-face 'tab-line-highlight
                      'help-echo "Click to add tab"))

    (add-list-to-list 'tab-line-exclude-modes
                      '(ediff-mode
                        process-menu-mode
                        term-mode
                        vterm-mode
                        treemacs-mode
                        imenu-list-major-mode
                        ))
    (delq nil (delete-dups tab-line-exclude-modes))

    ;; (dolist (mode '(ediff-mode
    ;;                 process-menu-mode
    ;;                 term-mode
    ;;                 vterm-mode
    ;;                 treemacs-mode
    ;;                 imenu-list-major-mode
    ;;                 ))
    ;;   (add-to-list 'tab-line-exclude-modes mode))

    (defun xy--tab-line-close-tab (&optional e)
      "Close the selected tab.

If tab is presented in another window, close the tab by using
`bury-buffer` function.  If tab is unique to all existing
windows, kill the buffer with `kill-buffer` function.  Lastly, if
no tabs left in the window, it is deleted with `delete-window`
function."
      (interactive "e")
      (let* ((posnp (event-start e))
             (window (posn-window posnp))
             (buffer (get-pos-property 1 'tab (car (posn-string posnp)))))
        (with-selected-window window
          (let ((tab-list (tab-line-tabs-window-buffers))
                (buffer-list (flatten-list
                              (seq-reduce (lambda (list window)
                                            (select-window window t)
                                            (cons (tab-line-tabs-window-buffers) list))
                                          (window-list) nil))))
            (select-window window)
            (if (> (seq-count (lambda (b) (eq b buffer)) buffer-list) 1)
                (progn
                  (if (eq buffer (current-buffer))
                      (bury-buffer)
                    (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
                    (set-window-next-buffers window (delq buffer (window-next-buffers))))
                  (unless (cdr tab-list)
                    (ignore-errors (delete-window window))))
              (and (kill-buffer buffer)
                   (unless (cdr tab-list)
                     (ignore-errors (delete-window window)))))))))

    (defun xy--tab-line-name-buffer (buffer &rest _buffers)
      "Create name for tab with padding and truncation.

If buffer name is shorter than `tab-line-tab-max-width' it gets
centered with spaces, otherwise it is truncated, to preserve
equal width for all tabs.  This function also tries to fit as
many tabs in window as possible, so if there are no room for tabs
with maximum width, it calculates new width for each tab and
truncates text if needed.  Minimal width can be set with
`tab-line-tab-min-width' variable."
      (with-current-buffer buffer
        (let* ((window-width (window-width (get-buffer-window)))
               (tab-amount (length (tab-line-tabs-window-buffers)))
               (window-max-tab-width (if (>= (* (+ tab-line-tab-max-width 3) tab-amount) window-width)
                                         (/ window-width tab-amount)
                                       tab-line-tab-max-width))
               (tab-width (- (cond ((> window-max-tab-width tab-line-tab-max-width)
                                    tab-line-tab-max-width)
                                   ((< window-max-tab-width tab-line-tab-min-width)
                                    tab-line-tab-min-width)
                                   (t window-max-tab-width))
                             3)) ;; compensation for ' x ' button
               (buffer-name (string-trim (buffer-name)))
               (name-width (length buffer-name)))
          (if (>= name-width tab-width)
              (concat  " " (truncate-string-to-width buffer-name (- tab-width 2)) "…")
            (let* ((padding (make-string (+ (/ (- tab-width name-width) 2) 1) ?\s))
                   (buffer-name (concat padding buffer-name)))
              (concat buffer-name (make-string (- tab-width (length buffer-name)) ?\s)))))))

    ;;   (defun xy--tab-line-name-buffer (buffer &rest _buffers)
    ;;     "Create name for tab with padding and truncation.

    ;; If buffer name is shorter than `tab-line-tab-max-width' it gets
    ;; centered with spaces, otherwise it is truncated, to preserve
    ;; equal width for all tabs.  This function also tries to fit as
    ;; many tabs in window as possible, so if there are no room for tabs
    ;; with maximum width, it calculates new width for each tab and
    ;; truncates text if needed.  Minimal width can be set with
    ;; `tab-line-tab-min-width' variable."
    ;;     (with-current-buffer buffer
    ;;       (let ((buffer (string-trim (buffer-name)))
    ;;             (right-pad (if tab-line-close-button-show "" " ")))
    ;;         (propertize (concat " " buffer right-pad)
    ;;                     'help-echo (when-let ((name (buffer-file-name)))
    ;;                                  (abbreviate-file-name name))))))

    ;; (define-advice tab-line-select-tab (:after (&optional e) xy:tab-line-select-tab)
    ;;   (select-window (posn-window (event-start e))))
    ))

(defun ui/init-visual-fill-column ()
  (use-package visual-fill-column
    :ensure t
    :hook ((visual-line-mode . visual-fill-column-mode)
           (org-mode . visual-line-fill-column-mode)
           (text-mode . visual-line-fill-column-mode))
    :config
    (setq visual-fill-column-enable-sensible-window-split t)
    (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
    ))

(defun ui/init-adaptive-wrap ()
  (use-package adaptive-wrap
    :ensure t
    :hook ((visual-line-mode . adaptive-wrap-prefix-mode)
           (visual-line-fill-column-mode . adaptive-wrap-prefix-mode))
    ))
