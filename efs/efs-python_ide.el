(load-file "~/.emacs.d/private/efs/efs-defaults.el")

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments
        '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  ;; (lsp-enable-which-key-integration t)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(treemacs)

(use-package lsp-ivy
  :after lsp)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; FIXME: not working
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  ;; (general-define-key
  ;;   :keymaps 'lsp-mode-map
  ;;   :prefix lsp-keymap-prefix
  ;;   "d" '(dap-hydra t :wk "debugger"))
  )

(use-package flycheck)

(use-package flycheck-pos-tip
  :after flycheck)

(use-package eldoc)

(use-package projectile)

(use-package treemacs-projectile)

(use-package flycheck-projectile)

(use-package counsel-projectile)

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (python-shell-completion-native-disabled-interpreters '("python3"))
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package conda
  :after python-mode
  :init
  (setq-default mode-line-format
              (cons '(:exec conda-env-current-name) mode-line-format))
  :custom
  (conda-anaconda-home "/opt/miniconda3")
  (conda-env-home-directory "~/.conda/")
  :config
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode t)
  ;; if you want to automatically activate a conda environment on the opening of a file:
  (add-to-hook 'find-file-hook
               (lambda ()
                 (when (bound-and-true-p conda-project-env-path)
                   (conda-env-activate-for-buffer)))))

(use-package pytest
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-last-failed
               pytest-pdb-last-failed
               pytest-module
               pytest-pdb-module)
    :config
    (add-to-list 'pytest-project-root-files "setup.cfg"))

(use-package blacken)

(use-package pydoc)

(use-package live-py-mode
  :commands live-py-mode)
