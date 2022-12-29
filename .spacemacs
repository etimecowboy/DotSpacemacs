;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; Time-stamp: <2022-12-29 Thu 03:36 by xin on tufg>
;; This file is loaded by Spacemacs at startup.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(javascript
     asciidoc
     ruby
     perl5
     (auto-completion :variables
                      auto-completion-private-snippets-directory "~/.emacs.d/private/snippets"
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-minimum-prefix-length 2)
     (better-defaults :variable
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first t)
     (chinese :variables
              chinese-enable-youdao-dict t)
     (colors :variables
             colors-colorize-identifiers 'all)
     csv
     emacs-lisp
     common-lisp
     ;; semantic ;; FIXME: cause error to lsp-headerline-breadcrumb-mode
     (git :variables
          git-enable-magit-gitflow-plugin t)
     html
     compleseus
     (lsp :variables
          lsp-lens-enable t
          lsp-use-lsp-ui t
          lsp-modeline-code-actions-segments '(count icon)
          lsp-headerline-breadcrumb-enable t
          lsp-headerline-breadcrumb-icons-enable t
          lsp-headerline-breadcrumb-segments '(project file symbols)
          lsp-rust-server 'rust-analyzer)
     markdown
     graphviz
     (plantuml :variables
               plantuml-jar-path (expand-file-name "/opt/plantuml/plantuml.jar")
               org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
     (multiple-cursors :variables
                       multiple-cursors-backend 'mc)
     (spell-checking  :variables
                      spell-checking-enable-by-default t
                      enable-flyspell-auto-completion t
                      spell-checking-enable-auto-dictionary nil)
     (syntax-checking :variables
                      syntax-checking-use-original-bitmaps t)
     (version-control :variables
                      ;; version-control-diff-tool 'diff-hl
                      version-control-diff-tool 'git-gutter+
                      version-control-diff-side 'left
                      version-control-global-margin t)
     (treemacs :variables
               treemacs-use-git-mode 'deferred
               treemacs-lock-width t
               treemacs-is-never-other-window t
               treemacs-no-delete-other-windows t
               treemacs-use-all-the-icons-theme t
               )
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     (python :variables
             python-backend 'lsp
             ;; FIXME: it seems that pyright is preferred and I cannot use pylsp if both are installed.
             ;; python-lsp-server 'pyright ;; microsoft new python lsp client written in TypeScript
             python-lsp-server 'pylsp ;; python-lsp-server, written in python
             python-test-runner 'pytest
             python-formatter 'black
             python-save-before-test t)
     ;; ipython-notebook ;; replaced by jupyter package
     (conda :variables
            conda-anaconda-home "/opt/miniconda3"
            conda-env-home-directory "~/.conda/")
     octave
     (bibtex :variables
             bibtex-enable-ebib-support t
             ebib-preload-bib-files '("~/org/bib/all.bib")
             ebib-file-search-dir '("~/doc")
             ebib-import-directory "~/Downloads")
     (latex :variables
            ;; latex-backend 'company-auctex
            latex-backend 'lsp
            latex-build-command 'latexmk
            latex-build-engine 'xetex
            latex-view-pdf-in-split-window t
            latex-enable-folding t
            latex-refresh-preview t
            latex-enable-magic t
            magic-latex-enable-suscript nil
            magic-latex-enable-inline-image t)
     (sql :variables
          sql-backend 'lsp
          sql-lsp-sqls-workspace-config-path 'workspace
          sql-capitalize-keywords t
          sql-auto-indent nil)
     pdf
     epub
     (c-c++ :variables
            c-c++-backend 'lsp-ccls
            ;; ccls-executable "/snap/bin/ccls" ;; use system ccls package
            ;; c-c++-backend 'lsp-clangd
            ;; lsp-clients-clangd-executable "/usr/bin/clangd-10"
            c-c++-lsp-enable-semantic-highlight 'rainbow
            c-c++-lsp-semantic-highlight-method 'overlay
            c-c++-enable-google-style t
            c-c++-enable-google-newline t
            c-c++-adopt-subprojects t
            c-c++-dap-adapters '(dap-lldb dap-cpptools)
            c-c++-default-mode-for-headers 'c++-mode)
     (cmake :variables
            cmake-backend 'lsp
            cmake-enable-cmake-ide-support t)
     (dap :variables
          dap-enable-mouse-support t
          dap-python-debugger 'debugpy)
     (shell :variables
            shell-default-shell 'vterm
            shell-default-position 'bottom
            shell-default-height 20
            shell-default-full-span nil
            shell-default-term-shell "/bin/bash"
            multi-term-program "/bin/bash"
            close-window-with-terminal t)
     (shell-scripts :variables
                    shell-scripts-backend 'lsp)
     (docker :variables
             docker-dokerfile-backend 'lsp)
     (rust :variables
           rust-backend 'lsp)
     (ess :variables
          ess-r-backend 'lsp
          ess-assign-key "\M--")
     (spacemacs-layouts :variables
                        spacemacs-layouts-restricted-functions '(spacemacs/window-split-double-columns
                                                                 spacemacs/window-split-triple-columns
                                                                 spacemacs/window-split-grid)
                        spacemacs-layouts-restrict-spc-tab nil
                        persp-autokill-buffer-on-remove 'kill-weak)
     (xclipboard :variables
                 xclipboard-enable-cliphist t)
     (org :variables
          org-enable-github-support t
          org-enable-notifications t
          org-start-notification-daemon-on-startup t
          org-enable-org-contacts-support t
          org-enable-epub-support t
          org-enable-verb-support nil ;; try to solve ob-async error
          org-enable-appear-support t
          org-enable-roam-support t
          org-enable-roam-protocol t
          org-enable-roam-ui t
          org-enable-transclusion-support t
          ;; org-enable-reveal-js-support t
          ;; org-enable-hugo-support t
          ;; org-enable-sticky-header t ;; problematic in some cases
          ;; org-enable-valign t ;; problematic in some cases
          ;; org-projectile-file "TODOs.org" ;; I use a signle inbox file to record all todos
          ;; org-enable-roam-server nil ;; replaced by org-roam-ui
          ;; org-enable-asciidoc-support t ;; no use
          ;; TODO: setup my agenda day view as the startup buffer instead of *spacemacs*
          ;; org-persp-startup-org-file nil
          ;; org-persp-startup-with-agenda t
          )
     tmux
     yaml
     search-engine
     emoji ;; :variables company-emoji-insert-unicode t
           ;; it makes too much input latency after you type :
     systemd
     (clojure :variables
              clojure-enable-fancify-symbols t
              clojure-backend 'lsp
              clojure-enable-linters 'clj-kondo)
     ;; NOTE: deft canbe replace by helm-ag etc search.
     ;; NOTE: I only use the ligature package, moved to chinese-extra layer
     (unicode-fonts :variables
                    unicode-fonts-enable-ligatures t
                    unicode-fonts-ligature-modes '(text-mode prog-mode))
     eaf
     nixos
     (tree-sitter :variables
                  tree-sitter-indent-enable t
                  tree-sitter-fold-enable t)
     ;;----------------------------------------
     ;; private layers
     chinese-extra
     all-the-icons-extra
     eaf-extra
     tmux-extra
     shell-extra
     latex-extra
     org-extra
     english
     dired-extra
     emacs-lisp-extra
     jupyter
     emacs-demo
     compleseus-extra
     treemacs-extra
     hardhat
     subed
     git-extra
     search-engine-extra
     hyperbole
     tree-sitter-extra
     ;; ;; abandoned
     ;; lsp-bridge
     ;; ui-tweak
     ;; xwidgets
     ;; emms
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(vi-tilde-fringe
     org-projectile
     org-jira
     ox-jira
     org-trello
     org-brain
     org-journal
     org-asciidoc
     ;; window-purpose ;; FIXME: excluded for the conflict with
     ;; `org-transclusion' live-sync edit, but no have to be included after helm
     ;; was removed
     unicode-fonts   ;; FIXME: no color emoji
     persistent-soft ;; FIXME: no color emoji
     ;; org-re-reveal
     ;; evil-org
     ;; evil-surround
     ;; pyim ;; use rime instead
     ;; chinese-wbim ;; use rime instead
     ;; hl-todo
     company-emoji
     ;; excludes all evil packages
     ;; evil
     ;; evil-tex
     ;; evil-visualstar
     ;; evil-visual-mark-mode
     ;; evil-unimpaired
     ;; evil-tutor
     ;; evil-textobj-line
     ;; evil-surround
     ;; evil-org
     ;; evil-numbers
     ;; evil-nerd-commenter
     ;; evil-matchit
     ;; evil-lisp-state
     ;; evil-lion
     ;; evil-indent-plus
     ;; evil-iedit-state
     ;; evil-goggles
     ;; evil-exchange
     ;; ;; evil-evilified-state
     ;; evil-escape
     ;; evil-ediff
     ;; evil-collection
     ;; evil-cleverparens
     ;; evil-args
     ;; evil-anzu
     )

   ;; Defines the behavior of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 60

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 102400 102400)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((projects . 10)
                                (bookmarks . 15)
                                (recents . 20))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable t

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(modus-vivendi
                         modus-operandi
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.2)
   ;; dotspacemacs-mode-line-theme '(spacemacs :separator none :separator-scale 0.8)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   ;; Commented for a test:
   ;; dotspacemacs-default-font '("Source Code Pro"
   ;;                             :size 10.0
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.1)
   ;; dotspacemacs-default-font '("Iosevka Nerd Font Mono"
   ;;                             :size 12.0
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.1)
   ;; dotspacemacs-default-font '("Fira Code Retina"
   ;;                             :size 11
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.2)
   ;; dotspacemacs-default-font '("Consolas"
   ;;                             :size 11.0
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.1)
   dotspacemacs-default-font '("Cascadia Code"
                               :size 11.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; dotspacemacs-default-font '("Cascadia Code"
   ;;                             :height 110
   ;;                             :powerline-scale 1.1)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.2

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 80

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols 'display-graphic-p

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers
     '(:relative nil
       :visual nil
       :disabled-for-modes dired-mode
                           doc-view-mode
                           markdown-mode
                           org-mode
                           pdf-view-mode
                           text-mode
       :size-limit-kb 1000)

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%a@%t | %U@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  (setenv "LANG" "")
  (setenv "LANGUAGE" "")
  (setenv "LC_ALL" "")
  (setenv "LC_CTYPE" "en_US.UTF-8")
  (setenv "LC_MESSAGES" "en_US.UTF-8")
  (setenv "LC_TIME" "C")
  (setenv "DICTIONARY" "en_US"))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq user-full-name "Xin Yang"
        user-mail-address "xin2.yang@gmail.com")
  (setq warning-minimum-level :emergency) ;; disable common warnings
  (setq max-lisp-eval-depth 10000)  ;; increase eval depth
  (setq auto-window-vscroll nil)    ;; reduce function calls
  ;;   "Directory where my emacs working files reside.")

  ;; layer: chinese
  ;; Use elpa mirrors, check README.org in the chinese layer directory.
  ;; (setq configuration-layer-elpa-archives
  ;;   '(("melpa-cn" . "http://mirrors.bfsu.edu.cn/elpa/melpa/")
  ;;     ("org-cn" . "http://mirrors.bfsu.edu.cn/elpa/org/")
  ;;     ("gnu-cn" . "http://mirrors.bfsu.edu.cn/elpa/gnu/")
  ;;     ("non-gnu" . "https://elpa.nongnu.org/nongnu/")))
  ;; ;; tuna mirrors
  ;; (setq configuration-layer-elpa-archives
  ;;       `(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ;;         ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
  ;;         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
  ;;         ("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;;         ;; ("sunrise-commander"  .  "https://mirrors.tuna.tsinghua.edu.cn/elpa/sunrise-commander/")
  ;;         ))

  ;; set some keys
  (spacemacs/set-leader-keys "jp" 'ace-pinyin-jump-char)
  (spacemacs/set-leader-keys "jP" 'ace-pinyin-jump-word)

  ;; set time locale to standard format, avoid chinese time stamps in org mode.
  (setq-default system-time-locale "C") ;; also can be solved by (setenv "LC_ALL" "C")

  ;; load custom-file
  (setq custom-file (concat user-emacs-directory "private/custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Automatically update timestamp of files
  (setq time-stamp-start "Time-stamp:"
        time-stamp-end "\n"
        time-stamp-format " <%Y-%02m-%02d %3a %02H:%02M by %u on %s>"
        time-stamp-time-zone t)
  (add-hook 'write-file-hooks #'time-stamp)

  ;; disable current-line highlight
  (spacemacs/toggle-highlight-current-line-globally-off)

  ;; add shrink-window (vertically) keys
  ;; exsiting keys:
  ;; enlarge-window C-x ^
  ;; enlarge-window-horizontally C-x }
  ;; shrink-window-horizontally C-x {
  (global-set-key (kbd "C-x %") 'shrink-window)

  ;; use google-chrome as my default web browser.
  (xy/set-google-chrome-as-default)
)
