(spacemacs/declare-prefix "aD" "demo")

(spacemacs/set-leader-keys
  "aDc" 'command-log-mode
  "aDl" 'clm/toggle-command-log-buffer
  "aDC" 'clm/command-log-clear
  "aDS" 'clm/command-log-save
  "aDg" 'gif-screencast-start-or-stop
  "aDt" 'keycast-tab-bar-mode)
