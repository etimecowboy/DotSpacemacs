(spacemacs/declare-prefix "d" "demo")

(spacemacs/set-leader-keys
  "dc" 'global-command-log-mode
  "dl" 'clm/toggle-command-log-buffer
  "dC" 'clm/command-log-clear
  "dS" 'clm/save-command-log
  "dg" 'gif-screencast-start-or-stop
  "dk" 'keycast-header-line-mode
  "dz" 'xy/toggle-focus
  "dZ" 'xy/toggle-demo
  "dt" 'xy/toggle-org-tree-slide
  "TS" 'xy/toggle-line-spacing
  "Tz" 'xy/toggle-focus
  "TZ" 'xy/toggle-demo
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
  ;; "ms" 'demo-it-start
  ;; "mq" 'demo-it-end
  ;; "mS" 'org-tree-slide-mode
  "Ts" 'org-tree-slide-mode
  "TS" 'org-tree-slide-skip-done-toggle)

;; (global-set-key (kbd "C-<f7>") 'gif-screencast-start-or-stop)
(global-set-key (kbd "<f11>") 'xy/toggle-focus)
(global-set-key (kbd "S-<f11>") 'xy/toggle-demo)
