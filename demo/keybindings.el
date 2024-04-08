(spacemacs/declare-prefix "aD" "demo")

(spacemacs/set-leader-keys
  "aDc" 'command-log-mode
  "aDl" 'clm/toggle-command-log-buffer
  "aDC" 'clm/command-log-clear
  "aDS" 'clm/command-log-save
  "aDg" 'gif-screencast-start-or-stop
  "aDk" 'keycast-header-line-mode
  "TS"  'xy/toggle-line-spacing
  ;; ;; "tN"  'fancy-narrow-mode
  ;; "n C-d"  'fancy-narrow-to-defun
  ;; "n C-n"  'fancy-narrow-to-region
  ;; "n C-p"  'fancy-narrow-to-page
  ;; "n C-s"  'org-fancy-narrow-to-subtree
  ;; "n C-w"  'fancy-widen
  ;; "n C-b"  'org-fancy-narrow-to-block
  ;; "n C-e"  'org-fancy-narrow-to-element
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "ms" 'demo-it-start
  "mq" 'demo-it-end
  "Ts" 'org-tree-slide-mode
  "TS" 'org-tree-slide-skip-done-toggle)

(global-set-key (kbd "C-<f7>") 'gif-screencast-start-or-stop)
