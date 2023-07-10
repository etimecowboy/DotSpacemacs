(spacemacs/declare-prefix "aD" "demo")

(spacemacs/set-leader-keys
  "aDc" 'command-log-mode
  "aDl" 'clm/toggle-command-log-buffer
  "aDC" 'clm/command-log-clear
  "aDS" 'clm/command-log-save
  "aDg" 'gif-screencast-start-or-stop
  "aDk" 'keycast-header-line-mode
  "TS"  'xah-toggle-line-spacing
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "ms" 'demo-it-start
  "mq" 'demo-it-end)

(global-set-key (kbd "C-<f7>") 'gif-screencast-start-or-stop)
