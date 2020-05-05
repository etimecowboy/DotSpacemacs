;;; packages.el --- tmux Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq tmux-packages
      '(
        ;; official tmux layer packages
        ;; golden-ratio
        ;; (tmux :location local)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        tmux-pane
        emamux
        zoom-window
        ob-tmux
        ))

;; official tmux layer package settings
;; (defun tmux/post-init-golden-ratio ()
;;   (with-eval-after-load 'golden-ratio
;;     (add-to-list 'golden-ratio-extra-commands 'tmux-nav-left)
;;     (add-to-list 'golden-ratio-extra-commands 'tmux-nav-right)
;;     (add-to-list 'golden-ratio-extra-commands 'tmux-nav-up)
;;     (add-to-list 'golden-ratio-extra-commands 'tmux-nav-down)))
;;
;; (defun tmux/init-tmux ()
;;   "Initialize tmux"
;;   (use-package tmux))
;;
;; (defun tmux/post-init-golden-ratio ()
;;   (with-eval-after-load 'golden-ratio
;;     (add-to-list 'golden-ratio-extra-commands '(tmux-pane--windmove "up" "tmux select-pane -U"))
;;     (add-to-list 'golden-ratio-extra-commands '(tmux-pane--windmove "down" "tmux select-pane -D"))
;;     (add-to-list 'golden-ratio-extra-commands '(tmux-pane--windmove "left" "tmux select-pane -L"))
;;     (add-to-list 'golden-ratio-extra-commands '(tmux-pane--windmove "right" "tmux select-pane -R"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tmux/init-tmux-pane ()
  "Initialize tmux-pane"
  (use-package tmux-pane
    :ensure t
    :config
    (global-set-key (kbd "C-M-<up>") (lambda () (interactive) (tmux-pane--windmove "up"  "tmux select-pane -U")))
    (global-set-key (kbd "C-M-<down>") (lambda () (interactive) (tmux-pane--windmove "down"  "tmux select-pane -D")))
    (global-set-key (kbd "C-M-<left>") (lambda () (interactive) (tmux-pane--windmove "left" "tmux select-pane -L")))
    (global-set-key (kbd "C-M-<right>") (lambda () (interactive) (tmux-pane--windmove "right" "tmux select-pane -R")))

    ;; (global-set-key (kbd "C-M-<up>") (tmux-pane--windmove "up" "tmux select-pane -U"))
    ;; (global-set-key (kbd "C-M-<down>") (tmux-pane--windmove "down" "tmux select-pane -D"))
    ;; (global-set-key (kbd "C-M-<left>") (tmux-pane--windmove "left" "tmux select-pane -L"))
    ;; (global-set-key (kbd "C-M-<right>") (tmux-pane--windmove "right" "tmux select-pane -R"))
    ))

(defun tmux/init-emamux ()
  "Initialize tmux-emamux"
  (use-package emamux
    :config
    (global-set-key (kbd "M-@") emamux:keymap)
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

(defun tmux/init-zoom-window ()
  "Initialize zoom-window"
  (use-package zoom-window
    :config
    (global-set-key (kbd "M-@ z") 'zoom-window-zoom)
    ))

(defun tmux/init-ob-tmux ()
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
    (org-babel-tmux-terminal "urxvt")
    (org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e"))
    ;; Finally, if your tmux is not in your $PATH for whatever reason, you
    ;; may set the path to the tmux binary as follows:
    ;; (org-babel-tmux-location "/usr/bin/tmux")
    ))
