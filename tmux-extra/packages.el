;;; packages.el --- tmux-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-12-26 Tue 12:51 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst tmux-extra-packages
  '(
    ;; tmux-pane
    emamux
    zoom-window
    ob-tmux
    org
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
    :config
    ;; FIXME: 2023-04-08 trying to remove all helm stuff
    ;; (setq emamux:completing-read-type 'helm)

    ;; NOTE: my tmux prefix key is also M-z, so I have to M-z M-z in terminal.
    (global-set-key (kbd "M-<f1>") emamux:keymap)
    (global-unset-key (kbd "M-z"))
    (global-set-key (kbd "M-Z") 'zap-to-char)

    ;; add tmux layer `tmux-nav-*' functions
    (define-key emamux:keymap (kbd "<left>") #'tmux-nav-left)
    (define-key emamux:keymap (kbd "<down>") #'tmux-nav-down)
    (define-key emamux:keymap (kbd "<up>") #'tmux-nav-up)
    (define-key emamux:keymap (kbd "<right>") #'tmux-nav-right)

    ;; add zoom-window package toggle
    (define-key emamux:keymap (kbd "z") #'zoom-window-zoom)

    ;; Change default emamux keys
    (define-key emamux:keymap (kbd "C-s") #'emamux:send-command)
    (define-key emamux:keymap (kbd "C-r") #'emamux:send-region)
    (define-key emamux:keymap (kbd ":") #'emamux:run-command)
    (define-key emamux:keymap (kbd "M-r") #'emamux:run-last-command)
    (define-key emamux:keymap (kbd "M-s") #'emamux:run-region)
    (define-key emamux:keymap (kbd "M-w") #'emamux:copy-kill-ring)
    (define-key emamux:keymap (kbd "C-y") #'emamux:yank-from-list-buffers)
    (define-key emamux:keymap (kbd "C-i") #'emamux:inspect-runner)
    (define-key emamux:keymap (kbd "C-c") #'emamux:interrupt-runner)
    (define-key emamux:keymap (kbd "C-l") #'emamux:clear-runner-history)
    (define-key emamux:keymap (kbd "M-z") #'emamux:zoom-runner)
    (define-key emamux:keymap (kbd "k") #'emamux:close-runner-pane)
    (define-key emamux:keymap (kbd "c") #'emamux:new-window)
    (define-key emamux:keymap (kbd "C") #'emamux:clone-current-frame)
    (define-key emamux:keymap (kbd "1") #'emamux:close-panes)
    (define-key emamux:keymap (kbd "C-K") #'emamux:kill-session)
    (define-key emamux:keymap (kbd "2") #'emamux:split-window)
    (define-key emamux:keymap (kbd "3") #'emamux:split-window-horizontally)

    ;; NOTE: the following doesn't work -----------------------------------------------
    ;; :bind
    ;; ;; ("M-z" . 'emamux:keymap)
    ;; ;; ("M-Z" . zap-to-char) ;; Rebind the default "M-z" (zap-to-char) to "M-Z".
    ;; (:map emamux:keymap
    ;;       ("<left>"  . tmux-nav-left)
    ;;       ("<down>"  . tmux-nav-down)
    ;;       ("<up>"    . tmux-nav-up)
    ;;       ("<right>" . tmux-nav-right)
	  ;;       ("z"       . zoom-window-zoom))
    ;; --------------------------------------------------------------------------------

    (spacemacs/declare-prefix "aa" "tmux")
    (spacemacs/set-leader-keys
      "aa <left>"  'tmux-nav-left
      "aa <down>"  'tmux-nav-down
      "aa <up>"    'tmux-nav-up
      "aa <right>" 'tmux-nav-right
      "aa C-s" 'emamux:send-command
      "aa C-r" 'emamux:send-region
      "aa :"   'emamux:run-command
      "aa M-r" 'emamux:run-last-command
      "aa M-s" 'emamux:run-region
      "aa C-y" 'emamux:yank-from-list-buffers
      "aa C-i" 'emamux:inspect-runner
      "aa C-c"   'emamux:interrupt-runner
      "aa C-l" 'emamux:clear-runner-history
      "aa M-z"   'emamux:zoom-runner
      "aa k" 'emamux:close-runner-pane
      "aa c" 'emamux:new-window
      "aa C" 'emamux:clone-current-frame
      "aa 1" 'emamux:close-panes
      "aa C-K" 'emamux:kill-session
      "aa 2" 'emamux:split-window
      "aa 3" 'emamux:split-window-horizontally
      "aa z" 'zoom-window-zoom)
    ))

(defun tmux-extra/init-zoom-window ()
  "Initialize zoom-window"
  (use-package zoom-window
    :defer t
    :commands zoom-window-zoom
    ))

(defun tmux-extra/init-ob-tmux ()
  "Initialize ob-tmux"
  (use-package ob-tmux
    :defer t
    :config
    ;; Set the terminal that will pop-up.
    (cond ;; ordered according to my personal preference
     ;; foot terminal
     ((executable-find "footclient")
      (setq org-babel-tmux-terminal "footclient"
            org-babel-tmux-terminal-opts '("-T" "Tmux@Emacs"
                                           "-W" "82x18")))
     ;; kitty terminal
     ;;
     ;; NOTE: Connect to kitty `daemon' on abstract socket `mykitty'. Open a new
     ;; tab for the ob-tmux session.
     ((executable-find "kitty")
      (setq org-babel-tmux-terminal "kitty"
            org-babel-tmux-terminal-opts '("@"
                                           "--to" "unix:@mykitty"
                                           "launch"
                                           "--type" "tab"
                                           "--keep-focus")))

     ((executable-find "wezterm") ;; wezterm terminal
      (setq org-babel-tmux-terminal "wezterm"))

     ((executable-find "uxterm") ;; xterm-unicode
      (setq org-babel-tmux-terminal "uxterm"
            org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e")))

     ((executable-find "xterm") ;; xterm
      (setq org-babel-tmux-terminal "xterm"
            org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e")))

     ;; this is the default value
     ((executable-find "gnome-terminal") ;; gnome terminal
      (setq org-babel-tmux-terminal "gnome-terminal"
            org-babel-tmux-terminal-opts '("--")))

     (t (message "None of the supported terminal emulator was found.")))

    :custom
    (org-babel-default-header-args:tmux
     '((:results . "silent")	; Nothing to be output
       (:session . "default")	; The default tmux session to send code to
       (:socket  . nil)))		  ; The default tmux socket to communicate with

    ;; The tmux sessions are prefixed with the following string.
    ;; You can customize this if you like.
    (org-babel-tmux-session-prefix nil)

    ;; Path to the tmux binary
    ;; (org-babel-tmux-location "/usr/bin/tmux")
    ))

(defun tmux-extra/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'ob-tmux)
  ))
