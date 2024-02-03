;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- media layer packages file for Spacemacs.
;; Time-stamp: <2024-02-01 Thu 10:13 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst media-packages
  '(emms
    emms-info-mediainfo
    org-emms
    subed
    (mpvi :location (recipe :fetcher github :repo "lorniu/mpvi"))
    (bilibili :location (recipe :fetcher github :repo "lorniu/bilibili.el"))
    ))

(defun media/init-emms ()
  (use-package emms
    :init
    (spacemacs|define-transient-state emms
      :title "emms transient state"
      :doc "
^Control^                    ^Playlist^                    ^Misc^
^^^^^^^^-------------------------------------------------------------------------
[_SPC_] pause                [_p_] play-playlist           [_b_] browse library
[_RET_/_ESC_] stop             [_c_] create-new              [_t_] show info
[_l_/_<right>_] forward        [_j_/_<down>_] next             [_m_] show status
[_h_/_<left>_]  backward       [_k_/_<up>_] previous           [_L_] load history
[_:_] seek-to                [_r_] random                  [_H_] save history
[_-_] volume-lower           [_R_] shuffle                 [_q_] quit
[_=_/_+_] volume-raise         [_g_] show playlist
[_f_] play-file              [_G_] popup playlist
[_d_] play-directory
"
      :bindings
      ("q" nil :exit t)
      ("j" emms-next)
      ("<down>" emms-next)
      ("k" emms-previous)
      ("<up>" emms-previous)
      ("h" emms-seek-backward)
      ("<left>" emms-seek-backward)
      ("l" emms-seek-forward)
      ("<right>" emms-seek-forward)
      (":" emms-seek-to)
      ("-" emms-volume-lower)
      ("=" emms-volume-raise)
      ("+" emms-volume-raise)
      ("t" emms-show)
      ("m" emms-display-modes)
      ("p" emms-play-playlist)
      ("f" emms-play-file)
      ("d" emms-play-directory)
      ("g" emms-playlist-mode-go)
      ("G" emms-playlist-mode-popup)
      ("b" emms-browser)
      ("R" emms-shuffle)
      ("r" emms-random)
      ("c" emms-playlist-new)
      ("L" emms-history-load)
      ("H" emms-history-save)
      ("SPC" emms-pause)
      ("RET" emms-stop)
      ("ESC" emms-stop)
      )

    :config
    (require 'emms-setup)
    (require 'emms-player-simple)
    (require 'emms-history)
    (emms-all)
    (emms-default-players)
    (emms-history-load)
    (setq emms-source-file-default-directory "~/音乐"
          emms-volume-change-function 'emms-volume-pulse-change
          emms-player-list '(emms-player-mpv)
          emms-player-mpv-use-playlist-option t
          emms-playlist-mode-center-when-go t
          emms-playlist-mode-window-width 50
          )
    ))

(defun media/init-emms-info-mediainfo ()
  (use-package emms-info-mediainfo))

(defun media/init-org-emms ()
  (use-package org-emms))

(defun media/init-subed ()
  (use-package subed
    :init
    ;; Disable automatic movement of point by default
    ;; (add-hook 'subed-mode-hook 'subed-disable-sync-point-to-player)
    ;; Remember cursor position between sessions
    (add-hook 'subed-mode-hook 'save-place-local-mode)))

(defun media/init-mpvi ()
  (use-package mpvi
    ;; :init
    ;; NOTE: define transient state
    ;; (spacemacs|define-transient-state mpvi
;;       :title "mpvi transient state"
;;       :doc "
;; ^Link^              ^Timestamp^
;; ^^^^^^^^---------------------------------------
;; [_,_] open
;; [_SPC_] pause       [_a_] update start
;; [_c_] clip          [_b_] update end
;; [_s_] seek          [_v_] preview
;; [_q_] quit
;; "
;;       :bindings
;;       ("q" nil :exit t)
;;       ("," org-open-at-point)
;;       ("SPC" mpvi-pause)
;;       ("c" mpvi-clip)
;;       ;; ("s" xy/mpvi-current-link-seek)
;;       ("s" spacemacs/mpvi-seek-transient-state/body)
;;       ("a" mpvi-insert)
;;       ("b" mpvi-current-link-update-end-pos)
;;       ("v" mpvi-current-link-show-preview))

;;     (spacemacs|define-transient-state mpvi-seek
;;       :title "mpvi-seek transient state"
;;       :doc "
;; ^Playback Control^            ^Subtitle^          ^Capture^                   
;; ^^^^^^^^----------------------------------------------------------------------------------
;; [_SPC_] pause
;; [_n_/_C-n_] forward 1s        [_T_] Load          [_c_/_C-c_] clip video
;; [_p_/_C-p_] backward 1s       [_t_/_C-t_] copy    [_s_] save screenshot as ...   
;; [_N_]    forward 1 percent                        [_C-s_] save screenshot to clipboard
;; [_P_]    backward 1 percent                       [_C-i_] save screenshot as attachment
;; [_M-n_]  forward 1 frame                          [_r_/_C-r_] orc result save to kill ring
;; [_M-p_]  backward 1 frame                         [_i_] insert current timestamp to buffer
;; [_C-l_]  beginning                                [_g_] insert current timestamp to minibuffer
;; [_M-<_]  beginning
;; [_k_]    faster
;; [_j_]    slower
;; [_l_]    normal speed
;; [_o_/_C-o_] open externally
;; [_v_/_C-v_] show playlist
;; [_q_]       quit
;; "
;;       :bindings
;;       ("q" nil :exit t)
;;       ("SPC" mpvi-seeking-pause)
;;       ("i"   mpvi-seeking-insert)
;;       ("g"   mpvi-seeking-revert)
;;       ("n"   (lambda () (interactive) (mpvi-seeking-walk 1)))
;;       ("p"   (lambda () (interactive) (mpvi-seeking-walk -1)))
;;       ("N"   (lambda () (interactive) (mpvi-seeking-walk "1%")))
;;       ("P"   (lambda () (interactive) (mpvi-seeking-walk "-1%")))
;;       ("M-n" (lambda () (interactive) (mpvi-seeking-walk :ff)))
;;       ("M-p" (lambda () (interactive) (mpvi-seeking-walk :fb)))
;;       ("C-l" (lambda () (interactive) (mpvi-seeking-walk 0)))
;;       ("C-n" (lambda () (interactive) (mpvi-seeking-walk 1)))
;;       ("C-p" (lambda () (interactive) (mpvi-seeking-walk -1)))
;;       ("M-<" (lambda () (interactive) (mpvi-seeking-revert 0)))
;;       ("k"   (lambda () (interactive) (mpvi-speed 1)))
;;       ("j"   (lambda () (interactive) (mpvi-speed -1)))
;;       ("l"   (lambda () (interactive) (mpvi-speed nil)))
;;       ("v"   mpvi-current-playing-switch-playlist)
;;       ("C-v" mpvi-current-playing-switch-playlist)
;;       ("c"   mpvi-seeking-clip)
;;       ("C-c" mpvi-seeking-clip)
;;       ("s"   mpvi-seeking-capture-save-as)
;;       ("C-s" mpvi-seeking-capture-to-clipboard)
;;       ("C-i" mpvi-seeking-capture-as-attach)
;;       ("r"   mpvi-seeking-ocr-to-kill-ring)
;;       ("C-r" mpvi-seeking-ocr-to-kill-ring)
;;       ("t"   mpvi-seeking-copy-sub-text)
;;       ("C-t" mpvi-seeking-copy-sub-text)
;;       ("T"   mpvi-current-playing-load-subtitle)
;;       ("SPC" mpvi-seeking-pause)
;;       ("o"   mpvi-current-playing-open-externally)
;;       ("C-o" mpvi-current-playing-open-externally)
;;       ("q"   abort-minibuffers)
;;       ("C-q" abort-minibuffers))
    
    :config
    (setq mpvi-favor-paths '("~/视频"
                             "~/音乐"
                             "~/zbox_sshfs"
                             "/media/xin")
          mpvi-attach-link-attrs "#+ATTR_ORG: :width 240\n#+ATTR_HTML: :width 320"

          ;; mpvi-ytdlp-extra-args "-c '~/.cache/cookies/cookies.txt'"

          ;; NOTE: no need to open html links with mpvi
          ;; mpvi-org-https-link-rules '("www.bilibili.com/"
          ;;                             "www.youtube.com/"
          ;;                             "www.youku.com/"
          ;;                             "www.pronhub.com/"
          ;;                             )
          )
    ;; Fix `mpvi-check-live' function problem after Emms updated to 18+ (2024)
    (setq emms-player-mpv-ipc-method 'ipc-server)
    ))

(defun media/init-bilibili ()
  (use-package bilibili
    :defer t
    :config
    (defconst bilibili-cookie-file "~/.cache/cookies/bilibili.string")

    ;; REF: http://xahlee.info/emacs/emacs/elisp_read_file_content.html
    (defun get-string-from-file (filePath)
      "Return file content as string."
      (with-temp-buffer
        (insert-file-contents filePath)
        (buffer-string)))

    (if (file-exists-p bilibili-cookie-file)
        (setq bilibili-cookie-text (get-string-from-file bilibili-cookie-file)))
    ))
