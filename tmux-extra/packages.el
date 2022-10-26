;;; packages.el --- tmux-extra layer packages file for Spacemacs.
;; Time-stamp: <2022-10-10 Mon 06:52 by xin on tufg>
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


;; :config
;; (global-set-key (kbd "M-z") emamux:keymap)
;; (global-set-key (kbd "M-Z") 'zap-to-char) ;; rebind the default "M-z"
;; ;; add tmux layer `tmux-nav-*' functions
;; ;; (define-key emamux:keymap (kbd "<left>") #'tmux-nav-left)
;; ;; (define-key emamux:keymap (kbd "<down>") #'tmux-nav-down)
;; ;; (define-key emamux:keymap (kbd "<up>") #'tmux-nav-up)
;; ;; (define-key emamux:keymap (kbd "<right>") #'tmux-nav-right)
(defun tmux-extra/init-emamux ()
  "Initialize tmux-emamux"
  (use-package emamux
    :ensure t
    :config
    (setq emamux:completing-read-type 'helm)
    ;; NOTE: my tmux prefix key is also M-z, so I have to M-z M-z in termianl.
    (global-set-key (kbd "M-z") emamux:keymap)
    (global-set-key (kbd "M-Z") 'zap-to-char)
    ;; add tmux layer `tmux-nav-*' functions
    (define-key emamux:keymap (kbd "<left>") #'tmux-nav-left)
    (define-key emamux:keymap (kbd "<down>") #'tmux-nav-down)
    (define-key emamux:keymap (kbd "<up>") #'tmux-nav-up)
    (define-key emamux:keymap (kbd "<right>") #'tmux-nav-right)
    ;; add zoom-window package toggle
    (define-key emamux:keymap (kbd "z") #'zoom-window-zoom)
    ;; add default emamux keys
    (define-key emamux:keymap (kbd "C-s") #'emamux:send-command)
    (define-key emamux:keymap (kbd "C-r") #'emamux:send-region)
    (define-key emamux:keymap (kbd ":") #'emamux:run-command)
    (define-key emamux:keymap (kbd "<backspace>") #'emamux:run-last-command)
    (define-key emamux:keymap (kbd "M-r") #'emamux:run-region)
    (define-key emamux:keymap (kbd "M-w") #'emamux:copy-kill-ring)
    (define-key emamux:keymap (kbd "C-y") #'emamux:yank-from-list-buffers)
    (define-key emamux:keymap (kbd "<tab>") #'emamux:inspect-runner)
    (define-key emamux:keymap (kbd "C-c") #'emamux:interrupt-runner)
    (define-key emamux:keymap (kbd "C-l") #'emamux:clear-runner-history)
    (define-key emamux:keymap (kbd "M-z") #'emamux:zoom-runner)
    (define-key emamux:keymap (kbd "k") #'emamux:close-runner-pane)
    (define-key emamux:keymap (kbd "c") #'emamux:new-window)
    (define-key emamux:keymap (kbd "C") #'emamux:clone-current-frame)
    (define-key emamux:keymap (kbd "1") #'emamux:close-panes)
    (define-key emamux:keymap (kbd "C-K") #'emamux:kill-session)
    (define-key emamux:keymap (kbd "-") #'emamux:split-window)
    (define-key emamux:keymap (kbd "|") #'emamux:split-window-horizontally)

    ;; ;; NOTE: the following doesn't work
    ;; :bind
    ;; ;; ("M-z" . 'emamux:keymap)
    ;; ;; ("M-Z" . zap-to-char) ;; Rebind the default "M-z" (zap-to-char) to "M-Z".
    ;; (:map emamux:keymap
    ;;       ("<left>"  . tmux-nav-left)
    ;;       ("<down>"  . tmux-nav-down)
    ;;       ("<up>"    . tmux-nav-up)
    ;;       ("<right>" . tmux-nav-right)
	  ;;       ("z"       . zoom-window-zoom))
    ;; (spacemacs/declare-prefix "M-z" emamux)
    ;; (spacemacs/set-leader-keys
    ;;   "M-z<left>" 'tmux-nav-left
    ;;   "M-z<down>" 'tmux-nav-down
    ;;   "M-z<up>" 'tmux-nav-up
    ;;   "M-z<right>" 'tmux-nav-right
    ;;   "M-zC-s" 'emamux:send-command
    ;;   "M-zC-y" 'emamux:yank-from-list-buffers
    ;;   "M-zM-!" 'emamux:run-command
    ;;   "M-zM-r" 'emamux:run-last-command
    ;;   "M-zM-s" 'emamux:run-region
    ;;   "M-zC-i" 'emamux:inspect-runner
    ;;   "M-zC-k" 'emamux:close-panes
    ;;   "M-zC-c" 'emamux:interrupt-runner
    ;;   "M-zM-k" 'emamux:clear-runner-history
    ;;   "M-zc" 'emamux:new-window
    ;;   "M-zC" 'emamux:clone-current-frame
    ;;   "M-z2" 'emamux:split-window
    ;;   "M-z3" 'emamux:split-window-horizontally)
    ))

(defun tmux-extra/init-zoom-window ()
  "Initialize zoom-window"
  (use-package zoom-window
    :ensure t
    :config
    ;; (define-key emamux:keymap (kbd "z") #'zoom-window-zoom)
    ;; (global-set-key (kbd "M-@ z") 'zoom-window-zoom)
    ))

(defun tmux-extra/init-ob-tmux ()
  "Initialize ob-tmux"
  (use-package ob-tmux
    ;; Install package automatically (optional)
    :ensure t
    :custom
    (org-babel-default-header-args:tmux
     '((:results . "silent")	; Nothing to be output
       (:session . "default")	; The default tmux session to send code to
       (:socket  . nil)))		  ; The default tmux socket to communicate with
    ;; The tmux sessions are prefixed with the following string.
    ;; You can customize this if you like.
    (org-babel-tmux-session-prefix nil)
    ;; The terminal that will be used.
    ;; You can also customize the options passed to the terminal.
    ;; The default terminal is "gnome-terminal" with options "--".
    ;; org-babel-tmux-terminal "uxterm"
    ;; (org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e"))
    (org-babel-tmux-terminal "kitty")
    (org-babel-tmux-terminal-opts '("@"
                                    "--to" "unix:@mykitty" ;; abstract socket
                                    "launch"
                                    "--type" "window"
                                    "--keep-focus"))
    ;; Finally, if your tmux is not in your $PATH for whatever reason, you
    ;; may set the path to the tmux binary as follows:
    ;; (org-babel-tmux-location "/usr/bin/tmux")
    ))
