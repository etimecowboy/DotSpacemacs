;;; packages.el --- tmux-extra layer packages file for Spacemacs.
;; Time-stamp: <2022-03-28  一 15:19 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq tmux-extra-packages
      '(
        ;; tmux-pane
        emamux
        zoom-window
        ob-tmux
        ))

;; (defun tmux-extra/init-tmux-pane ()
;;   "Initialize tmux-pane"
;;   (use-package tmux-pane
;;     :ensure t
;;     :config
;;     (spacemacs/set-leader-keys "tx" 'tmux-pane-mode)
;;     (spacemacs/declare-prefix-for-minor-mode 'tmux-pane-mode "x" "tmux")
;;     (spacemacs/set-leader-keys-for-minor-mode 'tmux-pane-mode
;;       "xr" 'tmux-pane-rerun
;;       "xv" 'tmux-pane-toggle-vertical
;;       "xh" 'tmux-pane-toggle-horizontal
;;       "xV" 'tmux-pane-open-vertical
;;       "xH" 'tmux-pane-open-horizontal
;;       "xc" 'tmux-pane-close)
;;     (setq tmux-pane-vertical-percent 50
;;           tmux-pane-vertical-percent 50)
;;     ))

(defun tmux-extra/init-emamux ()
  "Initialize tmux-emamux"
  (use-package emamux
    :ensure t
    :config
    (global-set-key (kbd "M-z") emamux:keymap)
    (global-set-key (kbd "M-Z") 'zap-to-char) ;; rebind the default "M-z"
    ;; add tmux layer `tmux-nav-*' functions
    (define-key emamux:keymap (kbd "<left>") #'tmux-nav-left)
    (define-key emamux:keymap (kbd "<down>") #'tmux-nav-down)
    (define-key emamux:keymap (kbd "<up>") #'tmux-nav-up)
    (define-key emamux:keymap (kbd "<right>") #'tmux-nav-right)
    ))

;; "Default keymap for emamux commands. Use like
;; \(global-set-key (kbd \"M-g\") emamux:keymap\)

;; Keymap:

;; | Key | Command                          |
;; |-----+----------------------------------|
;; | C-s | emamux:send-command              |
;; | C-y | emamux:yank-from-list-buffers    |
;; | M-! | emamux:run-command               |
;; | M-r | emamux:run-last-command          |
;; | M-s | emamux:region                    |
;; | C-i | emamux:inspect-runner            |
;; | C-k | emamux:close-panes               |
;; | C-c | emamux:interrupt-runner          |
;; | M-k | emamux:clear-runner-history      |
;; | c   | emamux:new-window                |
;; | C   | emamux:clone-current-frame       |
;; | 2   | emamux:split-window              |
;; | 3   | emamux:split-window-horizontally |
;; "

(defun tmux-extra/init-zoom-window ()
  "Initialize zoom-window"
  (use-package zoom-window
    :ensure t
    :config
    (define-key emamux:keymap (kbd "z") #'zoom-window-zoom)
    ;; (global-set-key (kbd "M-@ z") 'zoom-window-zoom)
    ))

(defun tmux-extra/init-ob-tmux ()
  "Initialize ob-tmux"
  (use-package ob-tmux
    ;; Install package automatically (optional)
    :ensure t
    :custom
    (org-babel-default-header-args:tmux
     '((:results . "silent")	;
       (:session . "default")	; The default tmux session to send code to
       (:socket  . nil)))		; The default tmux socket to communicate with
    ;; The tmux sessions are prefixed with the following string.
    ;; You can customize this if you like.
    (org-babel-tmux-session-prefix nil)
    ;; The terminal that will be used.
    ;; You can also customize the options passed to the terminal.
    ;; The default terminal is "gnome-terminal" with options "--".
    (org-babel-tmux-terminal "xterm-256color")
    (org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e"))
    ;; Finally, if your tmux is not in your $PATH for whatever reason, you
    ;; may set the path to the tmux binary as follows:
    ;; (org-babel-tmux-location "/usr/bin/tmux")
    ))
