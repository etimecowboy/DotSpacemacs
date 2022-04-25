;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; Time-stamp: <2022-04-25 Mon 17:16 by xin on tufg>
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
   '(themes-megapack
     perl5
     (auto-completion :variables
                      auto-completion-private-snippets-directory "~/.emacs.d/private/snippets"
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      company-emoji-insert-unicode nil ;; suggested by emoji layer
                      )
     (better-defaults :variable
                      better-defaults-move-to-end-of-code-first t)
     (chinese :variables
              ;;chinese-enable-fcitx t
              ;;chinese-fcitx-use-dbus t
              chinese-enable-youdao-dict t)
     (colors :variables
             colors-colorize-identifiers 'all
             ;; colors-enable-nyan-cat-progress-bar t
             )
     csv
     emacs-lisp
     (git :variables
          git-enable-magit-gitflow-plugin t)
     html
     helm
     (lsp :variables
          lsp-lens-enable t
          lsp-use-lsp-ui t
          lsp-modeline-code-actions-segments '(count icon)
          lsp-rust-server 'rust-analyzer
          )
     markdown
     graphviz
     (plantuml :variables
               plantuml-jar-path (expand-file-name "/opt/plantuml/plantuml.jar")
               org-plantuml-jar-path "/opt/plantuml/plantuml.jar"
               plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\"")
               plantuml-indent-level 4)
     (multiple-cursors :variables
                       multiple-cursors-backend 'mc)
     (spell-checking  :variables
                      spell-checking-enable-by-default nil
                      enable-flyspell-auto-completion t
                      spell-checking-enable-auto-dictionary nil)
     (syntax-checking :variables
                      syntax-checking-use-original-bitmaps t)
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-diff-side 'left
                      version-control-global-margin t)
     (treemacs :variables
               treemacs-use-git-mode 'deferred
               treemacs-lock-width t
               treemacs-is-never-other-window t
               treemacs-no-delete-other-windows t
               treemacs-use-all-the-icons-theme t)
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     (python :variables
             python-backend 'lsp
             python-formatter 'lsp
             python-lsp-server 'pyright
             python-test-runner 'pytest)
     ipython-notebook
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
            latex-backend 'lsp
            latex-build-command 'latexmk
            latex-build-command 'xetex
            latex-enable-folding t
            latex-refresh-preview t
            latex-enable-magic t)
     sql
     pdf
     epub
     (c-c++ :variables
            c-c++-backend 'lsp-ccls
            ccls-executable "/snap/bin/ccls"
            ;; c-c++-backend 'lsp-clangd
            ;; lsp-clients-clangd-executable "/usr/bin/clangd-10"
            c-c++-lsp-enable-semantic-highlight 'rainbow
            c-c++-lsp-semantic-highlight-method 'overlay
            c-c++-enable-google-style t
            c-c++-enable-google-newline t
            c-c++-adopt-subprojects t)
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
     (docker :variables
             docker-dokerfile-backend 'lsp)
     (rust :variables
           rust-backend 'lsp
           )
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
          org-enable-org-journal-support t
          org-enable-sticky-header t
          org-enable-epub-support t
          org-enable-verb-support t
          org-enable-valign t
          org-enable-appear-support t
          org-enable-roam-support t
          org-enable-roam-protocol t
          ;; org-projectile-file "TODOs.org"
          ;; org-enable-roam-server nil ;; replaced by org-roam-ui
          ;; org-enable-asciidoc-support t
          ;; org-enable-org-brain-support t ;; replaced by org-roam
          ;; org-enable-reveal-js-support t
          ;; TODO: setup my agenda day view as the startup buffer instead of *spacemacs*
          ;; org-persp-startup-org-file nil
          ;; org-persp-startup-with-agenda t
          )
     tmux
     yaml
     search-engine
     emoji
     systemd
     (clojure :variables
              clojure-enable-fancify-symbols t
              clojure-backend 'lsp
              clojure-enable-linters 'clj-kondo)
     ;; NOTE: deft canbe replace by helm-ag etc search.
     ;;----------------------------------------
     ;; private layers
     tmux-extra
     shell-extra
     org-extra
     english
     chinese-extra
     xwidgets
     emms
     ;; ui-tweak
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      subed
                                      hardhat
                                      helm-icons
                                      all-the-icons-ibuffer
                                      all-the-icons-dired
                                      gif-screencast
                                      command-log-mode
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(org-projectile
     org-jira
     ox-jira
     org-trello
     org-brain
     org-journal
     org-asciidoc
     org-re-reveal
     ;; evil-org
     ;; evil-surround
     ;; pyim ;; use rime instead
     ;; chinese-wbim ;; use rime instead
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
   dotspacemacs-startup-lists '((recents . 10)
                                (bookmarks . 10)
                                (projects . 10))

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
   dotspacemacs-scratch-buffer-persistent t

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable t

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
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
   dotspacemacs-default-font '("Iosevka Nerd Font Mono"
                               :size 12.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; dotspacemacs-default-font '("FiraCode Nerd Font Mono"
   ;;                             :size 12.0
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.2)

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
   dotspacemacs-which-key-delay 0.4

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
   dotspacemacs-loading-progress-bar t

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
   dotspacemacs-pretty-docs nil

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
  ;; (setq warning-minimum-level :emergency) ;; disable common warnings
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

  ;; layer: org
  (setq org-directory "~/org"
        ;; org-link-file-path-type 'absolute 
        org-id-locations-file "~/org/org-id-locations"
        org-default-notes-file "~/org/notes.org"
        org-roam-directory "~/org/roam"
        org-roam-db-location "~/org/org-roam.db"
        org-roam-dailies-directory "~/org/dailies"
        ;; org-download-image-dir "~/org/img"
        )
  )

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
        time-stamp-format " <%Y-%02m-%02d %3a %02H:%02M by %u on %s>")
  (add-hook 'write-file-hooks #'time-stamp)

  ;; disable current-line highlight
  (spacemacs/toggle-highlight-current-line-globally-off)

  ;; Set default fonts for GUI mode
  ;; Characters:
  ;; abcdefghijklmnopqrstuvwxyz
  ;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
  ;; oO08 iIlL1 {} [] g9qcGQ ~-+=>
  ;; Width test:
  ;; 123456789012345`!@#$%^&*()_+
  ;; 中文的宽度？。
  ;; (if window-system
  ;;     ;;(spacemacs//set-monospaced-font "Source Code Pro" "方正楷体_GBK" 10 10)
  ;;     ;; (xy/set-font-InputMonoCompressed)
  ;;     ;; (xy/set-font-Consolas)
  ;;     ;; (xy/set-font-DejaVuSansMono)
  ;;   )
  ;; (xy/set-font-SourceCodePro)
  (xy/set-font-Iosevka)
  ;; (xy/set-font-FiraCode)

  ;; add global transparency toggle keys
  (spacemacs/set-leader-keys "tY" 'spacemacs/toggle-transparency)
  (spacemacs/declare-prefix "tT" "transparency")
  (spacemacs/set-leader-keys "tTy" 'spacemacs/enable-transparency)
  (spacemacs/set-leader-keys "tTn" 'spacemacs/disable-transparency)
  (spacemacs/set-leader-keys "tT=" 'spacemacs/increase-transparency)
  (spacemacs/set-leader-keys "tT+" 'spacemacs/increase-transparency)
  (spacemacs/set-leader-keys "tT-" 'spacemacs/decrease-transparency)
  (spacemacs/set-leader-keys "tT_" 'spacemacs/decrease-transparency)

  ;; add shrink-window (vertically) keys
  ;; exsiting keys:
  ;; enlarge-window C-x ^
  ;; enlarge-window-horizontally C-x }
  ;; shrink-window-horizontally C-x {
  (global-set-key (kbd "C-x %") 'shrink-window)

  ;; layer: search-engine
  (setq ;; browse-url-default-browser 'browse-url-generic
        browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome")
  ; more keys for quicker search
  (spacemacs/set-leader-keys "awg" 'engine/search-google)
  (spacemacs/set-leader-keys "awi" 'engine/search-google-images)
  (spacemacs/set-leader-keys "awG" 'engine/search-github)
  (spacemacs/set-leader-keys "awb" 'engine/search-bing)
  (spacemacs/set-leader-keys "aww" 'engine/search-wikipedia)
  (spacemacs/set-leader-keys "awe" 'engine/search-melpa)
  (spacemacs/set-leader-keys "aws" 'engine/search-stack-overflow)
  (spacemacs/set-leader-keys "awy" 'engine/search-youtube)

  ;; layer: chinese
  ;; for chinese layer `youdao-dcitionary' package
  (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
  (spacemacs/declare-prefix "oc" "Chinese")
  (spacemacs/set-leader-keys "ocd" 'find-by-pinyin-dired)
  (spacemacs/set-leader-keys "occ" 'chinese-conv-replace)
  (spacemacs/set-leader-keys "ocC" 'chinese-conv)

  ;; layer: spell-checking
  (with-eval-after-load "ispell"
    ;; aspell works great, but hunspell is more accurate.
    ;; ispell-program-name "aspell"
    ;; ispell-dictionary "american"
    (setq ispell-program-name "hunspell")
    ;; ispell-set-spellchecker-params has to be called
    ;; before ispell-hunspell-add-multi-dic will work
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,en_GB")
    (setq ispell-dictionary "en_US,en_GB")
    (ispell-change-dictionary "en_US" t))

  ;; layer: markdown
  (add-hook 'markdown-mode-hook #'toc-org-mode)

  ;; layer: org
  ;; (setq org-directory "~/org"
  ;;       org-link-file-path-type 'absolute
  ;;       org-id-locations-file "~/org/org-id-locations"
  ;;       org-roam-directory "~/org/roam"
  ;;       org-roam-db-location "~/org/org-roam.db"
  ;;       org-roam-dailies-directory "~/org/dailies"
  ;;       org-download-image-dir "~/org/img")
  (setq org-link-frame-setup
          '((vm . vm-visit-folder-other-frame)
            (vm-imap . vm-visit-imap-folder-other-frame)
            (gnus . org-gnus-no-new-news)
            (file . find-file)
            (wl . wl-other-frame)))
  (when (not window-system)
    (setq org-file-apps
          '(("\\.mm\\'" . default)
            ("\\.x?html?\\'" . default)
            ("\\.pdf\\'" . default)
            ("\\.png\\'" . default)
            ("\\.jpg\\'" . default)
            ("\\.jpeg\\'" . default)
            ("\\.bmp\\'" . default)
            ("\\.svg\\'" . default)
            ("\\.mp3\\'" . default)
            ("\\.aac\\'" . default)
            ("\\.opus\\'" . default)
            ("\\.mp4\\'" . default)
            ("\\.mkv\\'" . default)
            (directory . emacs)
            (auto-mode . emacs))))

  ;;; load modules
  ;; FIXME: find a method to set org-modules
  ;; (with-eval-after-load 'org
  ;;   (add-to-list 'org-modules
  ;;                'ol-bbdb 'ol-bibtex 'ol-docview 'ol-info 'ol-man 'ol-w3m
  ;;                'ox-texinfo 'ox-man 'ox-koma-letter 'ox-beamer 'ox-org
  ;;                'org-id 'org-crypt 'org-protocol 'org-toc))
  ;; (setq org-modules
  ;;       '(;;;; org official lisps
  ;;         ol-bbdb ol-bibtex ol-docview ol-info ol-man ol-w3m
  ;;                 'ox-texinfo 'ox-man 'ox-koma-letter 'ox-beamer 'ox-org
  ;;                 org-id org-crypt org-protocol org-habit
  ;;                 ;; org-contrib
  ;;                 ;; org-attach-embedded-images ;; not working
  ;;                 ;; ol-gnus org-bookmark org-mew org-expiry
  ;;                 ;; org-git-link
  ;;                 org-toc
	;; 	              ))

  ;;; todo items
  (setq org-todo-keywords
        '(;; for tasks
          (sequence "TODO(t)" "SOMEDAY(x)" "NEXT(n)" "STARTED(s!)"
                    "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
          ;; for notes
          (sequence "NEW(a)" "REVIEW(r)" "|" "MARK(m!)" "USELESS(u!)")
          ))
  (setq org-use-fast-todo-selection t
        org-treat-insert-todo-heading-as-state-change t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t)

  ;;; todo hooks
  ;; todo entry automatically changes to DONE when all children are done
  ;; (defun org-summary-todo (n-done n-not-done)
  ;;   "Switch entry to DONE when all subentries are done, to TODO
  ;; otherwise."
  ;;   (let (org-log-done org-log-states)   ; turn off logging
  ;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  ;; (add-hook ('org-after-todo-statistics-hook #'org-summary-todo)
  ;; - REF: http://thread.gmane.org/gmane.emacs.orgmode/21402/focus=21413
  ;; - FIXME: `state' variable is not recognised
  ;; - TODO: waiting for a `after-schedule-hook' in future release.
  (add-hook 'org-after-todo-state-change-hook
            #'(lambda ()
                ;; Delete scheduled time after changing the state to SOMEDAY
                (if (or (string= org-state "SOMEDAY") (string= org-state "TODO"))
                   (org-remove-timestamp-with-keyword org-scheduled-string))
                ;; Automatically schedule the task to today after enter NEXT
                (if (string= org-state "NEXT")
                    (org-schedule nil "+0"))
                (if (string= org-state "DONE")
                    (alert "WELL DONE" :title "Agenda" :category 'Emacs :severity 'trivial))
                (if (string= org-state "CANCELLED")
                    (alert "Task Cancelled" :title "Agenda" :category 'Emacs :severity 'trivial))
               ))

  ;;; tags
  (setq org-stuck-projects '("+prj/-SOMEDAY-DONE" ("NEXT" "STARTED"))
        org-use-tag-inheritance t
        org-tags-exclude-from-inheritance '("prj" "crypt" "book"))

  ;;; properties
  ;; Don't inheritant property for sub-items, since it slows
  ;; down property searchings.
  (setq org-use-property-inheritance nil)

  ;;; additional properties
  ;; - NOTE: a task should not takes more than 4 hours (a half day),
  ;; otherwise you MUST break it into smaller tasks.
  (setq org-global-properties
        '(("Effort_ALL" .
           "0:02 0:10 0:30 1:00 1:30 2:00 2:30 3:00 4:00")
          ("Importance_ALL" .
           "A B C")
          ("SCORE_ALL" .
           "0 1 2 3 4 5 6 7 8 9 10")))

  ;;; priority
  ;; (setq org-enable-priority-commands           t
  ;;       org-highest-priority                  ?A
  ;;       org-lowest-priority                   ?C
  ;;       org-default-priority                  ?B
  ;;       org-priority-start-cycle-with-default  t)

  ;;; column view
  (setq org-columns-default-format ; Set default column view headings
        "%CATEGORY(Cat.) %PRIORITY(Pri.) %Importance(Imp.) \
%6TODO(State) %35ITEM(Details) %ALLTAGS(Tags) %5Effort(Plan){:} \
%6CLOCKSUM(Clock){Total} %SCORE(SCORE)")

  ;;; clock
  (setq org-clock-history-length 10
        org-clock-idle-time      15
        org-clock-in-resume      t
        org-clock-into-drawer    t
        org-clock-in-switch-to-state    "STARTED"
        org-clock-out-switch-to-state   "WAITING"
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done  t
        org-clock-persist        t
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-report-include-clocking-task t
        org-clock-persist-query-save t
        org-clock-sound t)

  ;;; logging
  (setq org-log-done            'time
        org-log-done-with-time  t
        org-log-into-drawer     t
        org-log-redeadline      'note
        org-log-reschedule      'time
        org-log-refile          'time
        org-log-state-notes-insert-after-drawers t)

  ;;; archieve
  ;; Infomation saved in archives
  (setq org-archive-save-context-info
        '(time file category todo priority itags olpath ltags))
  ;; Makes it possible to archive tasks that are not marked DONE
  (setq org-archive-mark-done nil)

  ;;; capture
  ;; REF:
  ;; - https://github.com/sprig/org-capture-extension
  ;; - https://github.com/sprig/org-capture-extension/issues/37
  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

  (setq org-capture-templates
        '(("t" "Capture a New Task from Emacs"
           entry (file+headline "~/org/roam/inbox.org" "Tasks")
           "** TODO %^{Task}
:LOGBOOK:
- Create time: %U
- From: %a
:END:

Resources: [add existing nodes here!]" :empty-lines 1 :prepend t :clock-keep t)

          ("n" "Take a Note from Emacs"
           entry (file+headline "~/org/roam/inbox.org" "Notes")
           "** NEW %^{Title}
:LOGBOOK:
- Create time: %U
- From: %a
:END:

Resources: [add existing nodes here!]" :empty-lines 1 :prepend t :clock-keep t)

          ("e" "English language study: phrases/sentences"
           entry (file+headline "~/org/roam/inbox.org" "English")
           "** %?
:LOGBOOK:
- Create time: %U
- From: %a
:END:"
           :empty-lines 1 :prepend t :clock-keep t)

          ("b" "Add a bookmark"
           entry (file+headline "~/org/roam/inbox.org" "Bookmark")
           "** %a
:PROPERTIES:
:SCORE: %?
:END:
:LOGBOOK:
- Create time: %U
- From: %a
:END:

- URL: %L
- Resources: [add existing nodes here!]
- Description:" :empty-lines 1 :prepend t :clock-keep t)

          ))

  ;;; refile
  ;; Targets include this file and any file contributing to the agenda
  ;; up to 3 levels deep
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3)))
  ;; Put the newest item on the top
  (setq org-reverse-note-order t)
  ;; Use paths for refile targets
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps t)
  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;;; org-attach
  (setq org-attach-archive-delete 'query
        org-attach-store-link-p 'attached)
  (require 'org-attach-git)
  ;; acctach from dired
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map
                (kbd "C-c C-x a")
                #'org-attach-dired-to-subtree)))
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "mf" 'xy/convert-attachment-to-file)

  ;;; babel
  ;; (setq org-src-fontify-natively t) ;; already in :init
  (setq org-confirm-babel-evaluate nil
        org-export-babel-evaluate nil
        org-src-tab-acts-natively t
        org-edit-src-turn-on-auto-save t
        org-adapt-indentation nil ;; it is not good for my snippets
        org-edit-src-content-indentation 2
        org-enable-fixed-width-editor t
        org-special-ctrl-o t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window
        org-src-ask-before-returning-to-edit-buffer nil)

  ;;; export
  (setq org-export-backends
        '(ascii beamer html latex man md odt org texinfo))
  (setq org-export-with-sub-superscripts '{})

  ;;; org-wild-notifier
  (setq alert-default-style 'libnotify
        org-wild-notifier-keyword-whitelist '("TODO" "NEXT" "STARTED" "WAITING"))

  ;;; org-agenda
  (setq org-agenda-dim-blocked-tasks nil
        org-agenda-window-frame-fractions '(0.20 . 0.80)
        ;; org-agenda-restore-windows-after-quit t
        org-agenda-window-setup  'current-window
        org-indirect-buffer-display 'current-window
        org-agenda-span 'week
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines nil
        org-agenda-todo-ignore-timestamp nil
        org-agenda-todo-ignore-with-date nil
        ;; Show all items when do a tag-todo search (C-c a M)
        ;; org-agenda-tags-todo-honor-ignore-options nil
        org-agenda-todo-list-sublevels nil
        org-agenda-include-deadlines t
        org-agenda-block-separator "========================================"
        org-agenda-use-time-grid t
        ;; FIXME: the custom time grid need to be fixed for this version of org
        ;; org-agenda-time-grid '((daily today require-timed) "----------------"
        ;;                        (800 1000 1200 1400 1600 1800 2000 2200))
        org-agenda-sorting-strategy
        '((agenda time-up category-keep priority-down todo-state-up)
          (todo time-up category-keep priority-down todo-state-up)
          (tags time-up category-keep priority-down todo-state-up)
          (search time-up category-keep priority-down todo-state-up)))

  ;; Custom agenda commands
  (setq org-agenda-custom-commands
        '(
          ("d" "Day Planner"
           ((agenda ""
                    (;; (org-agenda-sorting-strategy '(priority-down))
                     (org-agenda-span 1)
                     (org-agenda-deadline-warning-days 14)
                     (org-agenda-use-time-grid t)
                     (org-agenda-skip-scheduled-if-done t)
                     (org-agenda-skip-deadline-if-done t)
                     (org-agenda-skip-timestamp-if-done t)
                     (org-agenda-skip-archived-trees t)
                     (org-agenda-skip-comment-trees t)
                     (org-agenda-todo-list-sublevel t)
                     (org-agenda-timeline-show-empty-dates t)))

            ;;                 (tags-todo "TODO<>\"TODO\"+TODO<>\"SOMEDAY\"\
            ;; -SCHEDULED<=\"<+7d>\"-SCHEDULED>\"<+14d>\"-DEADLINE<=\"<+7d>\"-DEADLINE>\"<+14d>\"\
            ;; -repeat-bookmark-appt-note-en-prj"
            ;;                            ((org-agenda-overriding-header
            ;;                              "Pending Next Actions")
            ;;                             (org-tags-match-list-sublevels t)))

            (tags-todo "TODO<>\"TODO\"+TODO<>\"SOMEDAY\"-SCHEDULED<=\"<+7d>\"-SCHEDULED>\"<+14d>\"-DEADLINE<=\"<+7d>\"-DEADLINE>\"<+14d>\"-repeat-bookmark-appt-note-en"
                       ((org-agenda-overriding-header
                         "Pending Next Actions")
                        (org-tags-match-list-sublevels t)))

            (tags-todo "TODO=\"TODO\"-SCHEDULED-repeat"
                       ((org-agenda-overriding-header
                         "Task Inbox")
                        (org-tags-match-list-sublevels t)))

            (tags-todo "SCHEDULED>=\"<+1d>\"+SCHEDULED<=\"<+7d>\"-repeat-note-bookmark-en"
                       ((org-agenda-overriding-header
                         "Scheduled Tasks in 7 Days")
                        (org-tags-match-list-sublevels nil)))

            (tags-todo "TODO=\"SOMEDAY\""
                       ((org-agenda-overriding-header
                         "Future Work")
                        (org-tags-match-list-sublevels nil)))))

          ("w" "Weekly Review"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-agenda-ndays 7)
                     (org-agenda-deadline-warning-days 30)
                     (org-agenda-use-time-grid nil)
                     (org-agenda-skip-scheduled-if-done nil)
                     (org-agenda-skip-deadline-if-done nil)
                     (org-agenda-skip-timestamp-if-done nil)
                     (org-agenda-skip-archived-trees t)
                     (org-agenda-skip-comment-trees t)
                     (org-agenda-todo-list-sublevel t)
                     (org-agenda-timeline-show-empty-dates t)))

            (tags "CLOSED<\"<tomorrow>\"\
-repeat-sub-bookmark-note-idea-ARCHIVE"
                  ((org-agenda-overriding-header
                    "Archieve Closed Next Actions")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-skip-scheduled-if-done nil)
                   (org-agenda-skip-deadline-if-done nil)
                   (org-agenda-skip-timestamp-if-done nil)
                   (org-agenda-skip-archived-trees nil)))

            (tags "+note+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-7d>\""
                  ((org-agenda-overriding-header
                    "Review and Refile Notes in this week")
                   (org-tags-match-list-sublevels nil)))

            (tags "+bookmark+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-7d>\""
                  ((org-agenda-overriding-header
                    "Review and Refile bookmarks in this week")
                   (org-tags-match-list-sublevels nil)))

            (tags-todo "TODO<>\"TODO\"+TODO<>\"SOMEDAY\"\
-repeat-sub-bookmark-appt-note"
                       ;; "TODO<>\"TODO\"+SCHEDULED<\"<tomorrow>\"\
                       ;; +SCHEDULED>=\"<-1w>\"-repeat-prj"
                       ((org-agenda-overriding-header
                         "Process Pending Next Actions")
                        (org-tags-match-list-sublevels t)))

            (tags-todo "TODO=\"TODO\"-repeat"
                       ((org-agenda-overriding-header
                         "Schedule Tasks")
                        (org-tags-match-list-sublevels t)))

            (tags "prj/!-TODO-SOMEDAY-sub"
                  ((org-agenda-overriding-header
                    "Projects Review")
                   (org-tags-match-list-sublevels t)))))

          ("n" "Notes and in the Past 30 days" tags
           "+note+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-30d>\""
           ((org-agenda-overriding-header
             "Recent notes (30d)")
            (org-tags-match-list-sublevels nil)))

          ("b" "Bookmarks in the past 30 days" tags
           "+bookmark+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-30d>\""
           ((org-agenda-overriding-header
             "Recent bookmarks (30d)")
            (org-tags-match-list-sublevels nil)))

          ("l" "English study (30d)" tags
           "+en+TIMESTAMP_IA<\"<tomorrow>\"+TIMESTAMP_IA>=\"<-30d>\""
           ((org-agenda-overriding-header "English study")
            (org-tags-match-list-sublevels nil)))
          ))

  ;; Agenda view export C-x C-w
  (setq org-agenda-exporter-settings
        '((ps-number-of-columns 2)
          (ps-landscape-mode t)
          ;; (org-agenda-prefix-format " [ ] ")
          ;; (org-agenda-with-colors nil)
          ;; (org-agenda-remove-tags t)
          (org-agenda-add-entry-text-maxlines 5)
          (htmlize-output-type 'css)))

  ;;; org-plantuml
  (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar"
        org-plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\""))

  ;;; org-ditta
  (setq org-ditaa-jar-path "/opt/ditaa/ditaa.jar"
        org-ditaa-eps-jar-path "/opt/DitaaEps/DitaaEps.jar")

  ;;; org-download
  (setq org-download-method 'attach
        org-download-screenshot-method "scrot -s %s"
        org-download-image-org-width 400
        org-download-edit-cmd "krita %s"
        ;; org-download-heading-lvl nil
        ;; org-download-image-dir (concat org-directory "/img")
        ;; org-download-abbreviate-filename-function 'expand-file-name
        )
  (add-hook 'dired-mode-hook 'org-download-enable)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "iDe" 'org-download-edit)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "me" 'xy/org-download-edit)

  ;;; org-id
  ;; (setq org-id-locations-file (concat org-directory "/org-id-locations"))
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "iI" 'org-id-get-create)

  ;;; org-crypt
  (setq org-crypt-disable-auto-save 'encrypt
        org-crypt-key nil)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "E" 'org-encrypt-entry
    "D" 'org-decrypt-entry)

  ;;; org-roam
  (require 'org-roam-protocol)
  (setq org-roam-v2-ack t
        ;; org-roam-db-location (concat org-directory "/org-roam.db")
        ;; org-roam-dailies-directory (concat org-directory "/dailies")
        org-roam-completion-everywhere t
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-filetype "png"
        org-roam-protocol-store-links t
        org-roam-list-files-commands '(rg find)
        org-roam-mode-section-functions '(org-roam-backlinks-section
                                          org-roam-reflinks-section
                                          org-roam-unlinked-references-section))

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target (file+head
                                             "%<%Y%m%d%H%M%S>-${slug}.org"
                                             "#+title: ${title}
")
           :unnarrowed t :empty-lines 1 :prepend t)

          ("p" "project" plain "%?" :target (file+head
                                             "%<%Y%m%d%H%M%S>-${slug}.org"
                                             "#+title: ${title}
#+category: ${title}
#+filetags: PROJECT
* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:
") :unnarrowed t :empty-lines 1 :prepend t)))

  ;; NOTE: it seems argument switches doesn't work in org-roam-capture-ref-templates
  (setq org-roam-capture-ref-templates
        '(;; TODO: automatically download the original webpage as a compressed
          ;;       attachment using `org-web-tools-archieve'
          ("r" "ref" plain "%?" :target (file+head
                                         "${slug}.org"
                                         "#+title: ${title}
#+filetags: REF
* Local backup
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:

* Abstract
"
) :unnarrowed t :empty-lines 1 :jump-to-captured t)
            ;;
            ;; REF: https://www.zmonster.me/2020/06/27/org-roam-introduction.html
            ;; TODO:
            ;; 1. [X] creating an annotation headline in the existing note file
            ;; 2. [X] creating an annatation headline in a new node file
            ;; 3. [ ] same as 1&2 but under the ``Annotations'' headline.
            ;;
          ("a" "annote" plain "* %U %?
${body}
" :target (file+head "${slug}.org"
                     "#+title: ${title}
#+filetags: REF
* Local backup
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:

* Abstract
") :immediate-finish t :empty-lines 1 :jump-to-captured t)))

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %U %?" :target (file+head
                                                  "%<%Y-%m-%d>.org"
                                                  "#+title: %<%Y-%m-%d>
* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:
") :unnarrowed t :empty-lines 1 :prepend t)))

  (spacemacs/declare-prefix "aorR" "org-roam-ref")
  (spacemacs/declare-prefix-for-mode 'org-mode "rR" "org-roam-ref")
  (spacemacs/set-leader-keys
    "aorRa" 'org-roam-ref-add
    "aorRr" 'org-roam-ref-remove
    "aorRf" 'org-roam-ref-find
    "aorRo" 'org-roam-refile)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "rRa" 'org-roam-ref-add
    "rRr" 'org-roam-ref-remove
    "rRf" 'org-roam-ref-find
    "rRo" 'org-roam-refile)

  ;;; org-appear
  (setq org-appear-delay 0.8)

  ;;; org-sticky-header
  (setq org-sticky-header-full-path 'full)

  ;;; valign
  (setq valign-fancy-bar t)

  ;;; org-brain
  ;; (setq org-id-track-globally t
  ;;       org-brain-visualize-default-choices 'root
  ;;       org-brain-title-max-length 30
  ;;       org-brain-include-file-entries t
  ;;       org-brain-file-entries-use-title t
  ;;       org-brain-scan-for-header-entries t
  ;;       ;; org-brain-default-file-parent "brain"
  ;;       org-brain-scan-directories-recursively nil
  ;;       org-brain-backlink t
  ;; )

  ;;; toc-org
  (add-hook 'org-mode-hook #'toc-org-mode)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "o" 'org-toc-show)

  ;; layer: bibtex
  ;;; org-ref
  (setq bibtex-completion-bibliography '("~/org/bib/all.bib")
	      bibtex-completion-library-path '("~/doc/")
	      bibtex-completion-notes-path "~/org/roam/"
	      bibtex-completion-notes-template-multiple-files
        "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	      bibtex-completion-additional-search-fields '(keywords)
	      bibtex-completion-display-formats
	      '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	        (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	        (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	        (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	        (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	      bibtex-completion-pdf-open-function
	      (lambda (fpath)
	        (call-process "open" nil 0 nil fpath)))
  (setq bibtex-autokey-year-length 4
	      bibtex-autokey-name-year-separator "-"
	      bibtex-autokey-year-title-separator "-"
	      bibtex-autokey-titleword-separator "-"
	      bibtex-autokey-titlewords 2
	      bibtex-autokey-titlewords-stretch 1
	      bibtex-autokey-titleword-length 5)
  (setq org-ref-open-pdf-function
        (lambda (fpath)
          (start-process "zathura"
                         "*helm-bibtex-zathura*"
                         "/usr/bin/zathura" fpath)))
  (setq org-ref-bibliography-notes "~/org/ref_notes.org"
        org-ref-default-bibliography '("~/org/bib/ref.bib")
        org-ref-pdf-directory "~/doc")
  (setq reftex-default-bibliography '("~/org/bib/all.bib"))

  ;; layer: org-extra
  (add-hook 'org-babel-after-execute-hook #'xy/org-babel-after-execute)
  (add-hook 'after-save-hook #'org-redisplay-inline-images)
  ;; FIXME: try to solve cannot complete org-roam nodes
  ;; (add-hook 'org-mode-hook #'org-roam-update-org-id-locations) ;; too slow
  ;; (add-hook 'org-mode-hook #'org-roam-node-read--completions)
  ;; (add-hook 'org-mode-hook #'org-roam-buffer-refresh)
  ;; FIXME: Reload local settings when org file headings changed
  ;; (add-hook 'after-save-hook #'org-mode-restart)
  (add-hook 'org-agenda-mode-hook #'xy/org-roam-refresh-agenda-list)

  (add-to-list 'org-after-todo-state-change-hook
               '(lambda ()
                 (when (equal org-state "DONE")
                   (xy/org-roam-copy-todo-to-today))))

  (spacemacs/set-leader-keys "aorA" 'xy/org-roam-refresh-agenda-list)
  ;; (spacemacs/set-leader-keys "aorx" 'xy/org-roam-create-inbox-entry)
  ;; (spacemacs/set-leader-keys "aorX" 'xy/org-roam-create-new-project)
  (spacemacs/set-leader-keys "aorP" 'xy/org-roam-find-project)
  (spacemacs/set-leader-keys "aorS" 'xy/refresh-org-id-cache)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "rS" 'xy/refresh-org-id-cache
    "rE" 'org-roam-extract-subtree
    "rI" 'xy/org-roam-node-insert-immediate
    "rA" 'xy/org-roam-refresh-agenda-list
    "rP" 'xy/org-roam-find-project
    ;; "rx" 'xy/org-roam-create-inbox-entry
    ;; "rX" 'xy/org-roam-create-new-project
    "mu" 'xy/org-retrieve-url-from-point)

  ;; layer: git
  ;; TODO move to the layer
  ;; (global-git-commit-mode t)
  ;; (put 'helm-make-build-dir 'safe-local-variable 'stringp)

  ;; layer: treemacs, opens/closes files using ace
  (with-eval-after-load 'treemacs
    (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace)
    (treemacs-define-RET-action 'file-node-open #'treemacs-visit-node-ace)
    ;; REF: https://github.com/Alexander-Miller/treemacs/issues/842
    (add-hook 'treemacs-mode-hook
              #'(lambda ()
                  ;; (message "treemacs-mode-hook `%s'" (current-buffer))
                  ;; NOTE: Treemacs buffer in terminal mode cannot display
                  ;; scaled text.
                  (when window-system 
                    (text-scale-decrease 1)))))

  ;; layer: rcirc
  ;; (setq rcirc-server-alist
  ;;       '(("irc.freenode.net"
  ;;          :user-name "etimecowboy"
  ;;          :port "1337"
  ;;          :password "yang0213"
  ;;          :channels ("#emacs"))))
  ;; if authinfo support is enabled
  ;; (setq rcirc-server-alist
  ;;       '(("irc.freenode.net"
  ;;          :user-name "etimecowboy"
  ;;          :port "1337"
  ;;          :channels ("#emacs"))))

  ;; package: subed
  (use-package subed
    :init
    ;; Disable automatic movement of point by default
    ;; (add-hook 'subed-mode-hook 'subed-disable-sync-point-to-player)
    ;; Remember cursor position between sessions
    (add-hook 'subed-mode-hook 'save-place-local-mode))

  ;; package: dired
  (add-hook 'dired-mode-hook
            #'(lambda ()
                (dired-hide-details-mode t)
                (when window-system
                  (text-scale-decrease 1))))

  ;; package: undo-tree
  (setq undo-tree-auto-save-history nil)

  ;; package: hardhat
  (use-package hardhat
    :init
    ;; (space macs|diminish hardhat-mode "  ⓗ " " h")
    (global-hardhat-mode 1)
    :custom
    (setq hardhat-basename-protected-regexps
          '("~\\'"
            "\\.lock\\'"
            "\\.ix\\'"
            "\\`test\\.out\\'"
            "-autoloads\\.el\\'"
            "\\`Desktop\\.ini\\'"
            "\\`META\\.yml\\'"
            "\\`MYMETA\\.yml\\'"
            "\\`TAGS\\'"
            "\\`Thumbs\\.db\\'"
            "\\`\\.dropbox\\'"
            "\\`\\.dropbox\\.cache\\'"
            "\\`\\.emacs\\.desktop\\'"
            "\\`\\.emacs\\.desktop\\.lock\\'"
            "\\.orig\\'"
            "\\.rej\\'"
            "\\.bak\\'")
          hardhat-buffer-protected-functions
          '(hardhat-protected-by-ignoramus
            hardhat-protected-osx-homebrew
            (perl-mode . hardhat-protected-by-perl-semantic-eof)
            (cperl-mode . hardhat-protected-by-perl-semantic-eof))
          hardhat-fullpath-protected-regexps
          '("~/\\.emacs\\.d/elpa/"
            "~/\\.cpan/"
            "~/\\.cabal/"
            "~/perl5/perlbrew/"
            "~/\\.npm/"
            "~/\\.virtualenv/"
            "~/\\.virthualenv/"
            "~/\\.rvm/"
            "/[._]build/"
            "/\\.bzr/"
            "/\\.coverage/"
            "/\\.git/"
            "/\\.hg/"
            "/\\.rspec/"
            "/\\.sass-cache/"
            "/\\.svn/"
            "/_MTN/"
            "/_darcs/"
            "/CVS/"
            "/pm_to_blib/"
            "/RCS/"
            "/SCCS/"
            "/blib/"
            "/test_output/"
            "~/\\.emacs\\.d/\\.cask/"
            "~/\\.cask/"
            "~/\\.conda/"
            "~/bin/"
            "~/\\.tmux/"
            "~/\\.urxvt/"
            "~/\\.ssh/"
            "~/\\.docker/"
            "~/\\.cargo/"
            "~/\\.rustup/"
            "~/\\.local/")))

  ;; package: helm-icons
  (use-package helm-icons
    :init
    (helm-icons-enable)
    :custom
    (setq helm-icons-provider 'all-the-icons))

  ;; package: all-the-icons-ibuffer
  (use-package all-the-icons-ibuffer
    :init
    (all-the-icons-ibuffer-mode t))

  ;; package: all-the-icons-dired
  (use-package all-the-icons-dired
    :init
    (add-hook 'dired-mode-hook
              (lambda ()
                (if window-system
                    (all-the-icons-dired-mode -1)
                  (all-the-icons-dired-mode t))))
    ;; improve font rendering performance
    (setq inhibit-compacting-font-caches t))

  ;; package: gif-screencast
  (use-package gif-screencast
    :init
    (spacemacs/set-leader-keys "Cg" 'gif-screencast)
    :bind
    (:map gif-screencast-mode-map
          ("<f8>" . gif-screencast-toggle-pause)
          ("<f9>" . gif-screencast-stop)))

  ;; package: command-log-mode
  (use-package command-log-mode
    :init
    ;; (spacemacs/declare-prefix "Cl" "command-log")
    ;; (spacemacs/set-leader-keys "Clt" 'clm/toggle-command-log-buffer)
    ;; (spacemacs/set-leader-keys "Clc" 'clm/command-log-clear)
    ;; (spacemacs/set-leader-keys "Cls" 'clm/command-log-save)
    )
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" default))
 '(evil-want-Y-yank-to-eol nil)
 '(helm-completion-style 'helm)
 '(package-selected-packages
   '(helm-icons all-the-icons-completion all-the-icons-dired all-the-icons-ibuffer ignoramus hardhat zoom-window zonokai-emacs zenburn-theme zen-and-art-theme youdao-dictionary yasnippet-snippets yapfify yaml-mode xwwp-follow-link-helm xterm-color ws-butler writeroom-mode winum white-sand-theme which-key web-mode web-beautify volatile-highlights vi-tilde-fringe verb valign uuidgen use-package unkillable-scratch unfill undo-tree underwater-theme ujelly-theme typo-suggest twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-all-the-icons toxi-theme toc-org terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit systemd symon symbol-overlay sunny-day-theme sublime-themes subed subatomic256-theme subatomic-theme string-edit sql-indent sphinx-doc spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rime reverse-theme restart-emacs rebecca-theme rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme quickrun pytest pyim pyenv-mode pydoc py-isort purple-haze-theme pug-mode professional-theme prettier-js popwin poetry plantuml-mode planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persistent-scratch pdf-view-restore pcre2el password-generator paradox pangu-spacing ox-gfm ox-epub overseer orgit-forge organic-green-theme org-wild-notifier org-web-tools org-vcard org-superstar org-sticky-header org-roam-ui org-roam-bibtex org-rich-yank org-ref org-present org-pomodoro org-noter-pdftools org-mime org-fragtog org-fc org-emms org-download org-contrib org-cliplink org-appear open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-tmux ob-ipython ob-async nose noctilux-theme naquadah-theme nameless mwim mustang-theme multiple-cursors multi-vterm multi-term multi-line monokai-theme monochrome-theme molokai-theme moe-theme modus-themes mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow magic-latex-buffer madhat2r-theme macrostep lush-theme lsp-ui lsp-python-ms lsp-pyright lsp-origami lsp-latex lorem-ipsum live-py-mode link-hint light-soap-theme kaolin-themes journalctl-mode jbeans-theme jazz-theme ir-black-theme inspector inkpot-theme info+ indent-guide importmagic impatient-mode ibuffer-projectile hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-git-grep helm-flx helm-descbinds helm-ctest helm-css-scss helm-company helm-cider helm-c-yasnippet helm-bibtex helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme graphviz-dot-mode grandshell-theme gotham-theme google-translate google-c-style golden-ratio gnuplot gitignore-templates git-timemachine git-modes git-messenger git-link gh-md gendoxy gandalf-theme fuzzy font-lock+ flyspell-popup flyspell-correct-helm flycheck-ycmd flycheck-rtags flycheck-pos-tip flycheck-package flycheck-elsa flycheck-clj-kondo flx-ido flatui-theme flatland-theme find-by-pinyin-dired farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-terminal-cursor-changer evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-collection evil-cleverparens evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help engine-mode emr emojify emoji-cheat-sheet-plus emms-info-mediainfo emmet-mode emamux elisp-slime-nav elisp-def ein editorconfig dumb-jump drag-stuff dracula-theme dotenv-mode doom-themes dockerfile-mode docker django-theme disaster dired-quick-sort diminish diff-hl dictionary devdocs define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dap-mode dakrone-theme cython-mode cyberpunk-theme csv-mode cpp-auto-include conda company-ycmd company-web company-statistics company-rtags company-reftex company-quickhelp cvompany-plsense company-math company-emoji company-c-headers company-auctex company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode cmake-mode cmake-ide clues-theme clojure-snippets clean-aindent-mode cider-eval-sexp-fu chocolate-theme chinese-conv cherry-blossom-theme centered-cursor-mode ccls busybee-theme bubbleberry-theme browse-at-remote blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ace-pinyin ace-link ace-jump-helm-line ac-ispell))
 '(server-window 'pop-to-buffer))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
)
