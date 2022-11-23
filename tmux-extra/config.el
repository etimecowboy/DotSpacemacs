;; ;; NOTE: my tmux prefix key is also M-z, so I have to M-z M-z in termianl.
;; (global-set-key (kbd "M-z") emamux:keymap)

;; ;; REF: https://jao.io/blog/2022-06-08-slimmer-emacs-with-kitty.html
;; ;; get rid of the discontinuous vertical separators in Emacs:
;; (set-display-table-slot standard-display-table
;;                         'vertical-border (make-glyph-code ?│))
;; ;; clean up the end of the modeline
;; (setq mode-line-end-spaces nil)
