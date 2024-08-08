;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- Shell-extra Layer functions File for Spacemacs
;; Time-stamp: <2024-08-08 Thu 01:30:22 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; Add some more C- M- key sequences that used by terminal apps.
;; zellij lock-mode  prefix
(defun vterm-send-ctrl-g ()
  "Send `C-g' to the libvterm."
  (interactive)
  (vterm-send-key "g" nil nil t))


;; tmux default prefix
(defun vterm-send-ctrl-b ()
  "Send `C-b' to the libvterm."
  (interactive)
  (vterm-send-key "b" nil nil t))


(defun vterm-send-meta-return ()
  "Send `M-<return>' to the libvterm."
  (interactive)
  (vterm-send-key "<return>" nil t))


(defun xy/default-pop-tmux ()
  "Pop-up tmux in vterm."
  (require 'vterm)
  (require 'multi-vterm)
  (interactive)
  (let ((multi-vterm-program "tmux new-session -A -s default")
        (vterm-shell "tmux new-session -A -s default")
        (shell-default-shell 'vterm)
        )
    (spacemacs/default-pop-shell)))


(defun xy/default-pop-shell ()
  "Pop-up shell in vterm."
  (require 'vterm)
  (require 'multi-vterm)
  (interactive)
  (let ((multi-vterm-program "/bin/bash")
        (vterm-shell "/bin/bash")
        (shell-default-shell 'multi-vterm))
    (spacemacs/default-pop-shell)))


;; (defun term-mode-common-init ()
;;   "The common initialization for term."
;;   (setq-local scroll-margin 0)
;;   (setq-local truncate-lines t)
;;   )

;; 语法高亮显示
(defun eshell/bat (file)
  "cat FILE with syntax highlight."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (delay-mode-hooks
        (set-auto-mode)
        (font-lock-ensure)))
    (buffer-string)))

;; 交互式进入目录
(defun eshell/z ()
  "cd to directory with completion."
  (let ((dir (completing-read "Directory: " (ring-elements eshell-last-dir-ring) nil t)))
    (eshell/cd dir)))

;; ;; 查找文件
(defun eshell/f (filename &optional dir)
  "Search for files matching FILENAME in either DIR or the
current directory."
  (let ((cmd (concat
              ;; using find
              (executable-find "find")
              " " (or dir ".")
              " -not -path '*/.git*'"            ; ignore .git directory
              " -and -not -path 'build'"         ; ignore cmake build directory
              " -and -not -path '*/eln-cache*'"  ; ignore eln cache
              " -and -type f -and -iname "
              "'*" filename "*'")))
    (eshell-command-result cmd)))

;; REF: https://www.emacswiki.org/emacs/EshellAlias
;; FIXME: none of the below works
;; (defun eshell-load-bash-aliases ()
;;   "Read Bash aliases and add them to the list of eshell aliases."
;;   ;; Bash needs to be run - temporarily - interactively
;;   ;; in order to get the list of aliases.
;;   (interactive)
;;   (with-temp-buffer
;;     (call-process "bash" nil '(t nil) nil "-ci" "alias")
;;     (goto-char (point-min))
;;     (while (re-search-forward "alias \\(.+\\)='\\(.+\\)'$" nil t)
;;       (eshell/alias (match-string 1) (match-string 2)))))
;; (defun eshell-load-bash-aliases ()
;;   "Reads bash aliases from Bash and inserts them into the list of eshell aliases."
;;   (interactive)
;;   (progn
;;     (message "Parsing aliases")
;;     (shell-command "alias" "bash-aliases" "bash-errors")
;;     (switch-to-buffer "bash-aliases")
;;     (replace-string "alias " "")
;;     (goto-char 1)
;;     (replace-string "='" " ")
;;     (goto-char 1)
;;     (replace-string "'\n" "\n")
;;     (goto-char 1)
;;     (let ((alias-name) (command-string) (alias-list))
;;       (while (not (eobp))
;;         (while (not (char-equal (char-after) 32))
;;           (forward-char 1))
;;         (setq alias-name
;;               (buffer-substring-no-properties (line-beginning-position) (point)))
;;         (forward-char 1)
;;         (setq command-string
;;               (buffer-substring-no-properties (point) (line-end-position)))
;;         (setq alias-list (cons (list alias-name command-string) alias-list))
;;         (forward-line 1))
;;       (setq eshell-command-aliases-list alias-list))
;;     (if (get-buffer "bash-aliases")(kill-buffer "bash-aliases"))
;;     (if (get-buffer "bash-errors")(kill-buffer "bash-errors"))))

;; (defun xy/pretty-vterm-buffer ()
;;   (interactive)
;;   (when (display-graphic-p)
;;     (text-scale-decrease 1)
;;     ;; (text-scale-increase 1)
;;     ))

(defun xy/adapt-shell-config (&optional frame)
  "Adapt shell in Emacs to work in terminal or graphical environment."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (if (display-graphic-p frame)
      (progn
        ;; (when (featurep 'vterm)
        ;;   (setq vterm-shell "tmux new-session -A -s default"))
        ;; (when (featurep 'multi-vterm)
        ;;   (setq vterm-shell "tmux new-session -A -s default"
        ;;         multi-vterm-program "tmux new-session -A -s default"))
        ;; (text-scale-decrease 1)
        (setq line-spacing 0)
        (message "Adapt shell config for graphical frame."))
    (progn
      ;; (when (featurep 'vterm)
      ;;   (setq vterm-shell "/bin/bash"))
      ;; (when (featurep 'multi-vterm)
      ;;   (setq vterm-shell "/bin/bash"
      ;;         multi-vterm-program nil))
      (message "Adapt shell config for terminal frame."))))
