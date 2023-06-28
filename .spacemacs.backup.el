     ;; ---------------- disabled layers
     ;; (auto-completion
     ;;  :variables
     ;;  auto-completion-return-key-behavior nil
     ;;  auto-completion-tab-key-behavior 'complete
     ;;  auto-completion-complete-with-key-sequence nil
     ;;  auto-completion-complete-with-key-sequence-delay 0.1
     ;;  auto-completion-minimum-prefix-length 1
     ;;  auto-completion-idle-delay 0.2
     ;;  auto-completion-private-snippets-directory (concat
     ;;                                              user-emacs-directory
     ;;                                              "private/snippets")
     ;;  auto-completion-enable-snippets-in-popup t
     ;;  auto-completion-enable-help-tooltip t
     ;;  auto-completion-use-company-box nil ;; FIXME: <2023-04-04> error
     ;;  auto-completion-enable-sort-by-usage t
     ;;  :disabled-for
     ;;  python emacs-lisp c-c++ rust shell-script org
     ;;  )
     ;; (colors :variables ;; use tree-sitter instead
     ;;         ;; colors-colorize-identifiers 'all
     ;;         colors-enable-nyan-cat-progress-bar (display-graphic-p)) ;; I only want nyan cat
     ;; (lsp :variables  ;; use lsp-bridge instead
     ;;      lsp-lens-enable t
     ;;      lsp-use-lsp-ui t
     ;;      lsp-modeline-code-actions-segments '(count icon)
     ;;      lsp-headerline-breadcrumb-enable t
     ;;      lsp-headerline-breadcrumb-icons-enable t
     ;;      lsp-headerline-breadcrumb-segments '(project file symbols)
     ;;      lsp-rust-server 'rust-analyzer)
     ;; (dap :variables
     ;;      dap-enable-mouse-support t
     ;;      dap-python-debugger 'debugpy)
     ;; common-lisp
     ;; semantic ;; FIXME: cause error to lsp-headerline-breadcrumb-mode
     ;; octave
     ;; (ess :variables
     ;;      ess-r-backend 'lsp
     ;;      ess-assign-key "\M--")
     ;; (emoji :variables company-emoji-insert-unicode t)
     ;; emoji ;;TODO emoji-cheat-sheet-plus requires helm, which is to be removed
     ;; (clojure :variables
     ;;          clojure-enable-fancify-symbols t
     ;;          clojure-backend 'lsp
     ;;          clojure-enable-linters 'clj-kondo)
     ;; NOTE: deft canbe replace by helm-ag etc search.
     ;; NOTE: I only use the ligature package, moved to chinese-extra layer
     ;; (unicode-fonts
     ;;  :variables
     ;;  unicode-fonts-enable-ligatures nil ;; I don't like ligatures
     ;;  ;; unicode-fonts-ligature-modes '(text-mode prog-mode)
     ;;  )
     ;; tree-sitter ;; use official emacs29 package treesit.el instead.
     ;; (tree-sitter :variables
     ;;              spacemacs-tree-sitter-hl-black-list '(js2-mode rjsx-mode)
     ;;              tree-sitter-indent-enable t ;; experimental
     ;;              tree-sitter-fold-enable t ;; experimental
     ;; )
     ;; systemd
     ;; javascript
     ;; ruby
     ;; perl5
     ;; (version-control ;; this layer is too heavy.
     ;;  :variables
     ;;  ;; version-control-diff-tool 'diff-hl
     ;;  version-control-diff-tool 'git-gutter+
     ;;  version-control-diff-side 'left
     ;;  version-control-global-margin t)
     ;; (chinese ;; NOTE: it seems there is nothing useful to me
     ;;  :variables
     ;;  chinese-enable-youdao-dict t)
     ;; (sql
     ;;  :variables
     ;;  ;; sql-backend 'lsp
     ;;  sql-lsp-sqls-workspace-config-path 'workspace
     ;;  sql-capitalize-keywords t
     ;;  sql-auto-indent nil)
     ;; search-engine ;; NOTE: nothing useful to me
     ;; nixos
     ;; asciidoc
     ;; lua
     ;; rust
