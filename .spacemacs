;; -*- mode: emacs-lisp -*-
;; File path: ~/.spacemacs
;; Time-stamp: <2024-04-06 Sat 01:55 by xin on tufg>
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; This is the main init file of spacemacs
;;
;;; Code:

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
   dotspacemacs-enable-lazy-installation nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation nil

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path
   '(
     ;; "~/src/DotSpacemacs/"
     )

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '((spacemacs-layouts
      :variables
      layouts-enable-local-variables nil ;; Excluding `persp-mode' package would
                                         ;; cause `shell' layer problem if `t'
      )
     (better-defaults
      :variables
      better-defaults-move-to-beginning-of-code-first t
      better-defaults-move-to-end-of-code-first t)
     compleseus
     csv
     emacs-lisp
     (git
      :variables
      git-enable-magit-gitflow-plugin t)
     html
     markdown
     graphviz
     (plantuml
      :variables
      plantuml-jar-path (expand-file-name
                         "/opt/plantuml/plantuml.jar")
      org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
     (multiple-cursors
      :variables
      multiple-cursors-backend 'mc)
     (spell-checking
      :variables
      spell-checking-enable-by-default nil
      enable-flyspell-auto-completion t
      spell-checking-enable-auto-dictionary nil)
     (syntax-checking
      :variables
      syntax-checking-enable-tooltips nil)
     (treemacs
      :variables
      treemacs-use-git-mode 'deferred
      treemacs-lock-width t
      treemacs-is-never-other-window t
      treemacs-no-delete-other-windows t
      ;; treemacs-use-all-the-icons-theme t
      )
     (ibuffer
      :variables
      ibuffer-group-buffers-by 'projects)
     (bibtex
      :variables
      bibtex-enable-ebib-support t
      ebib-preload-bib-files '("~/org/bib/all.bib")
      ebib-file-search-dir '("~/doc")
      ebib-import-directory "~/Downloads")
     (latex
      :variables
      ;; latex-backend 'company-auctex
      ;; latex-backend 'lsp
      latex-build-command "LatexMk"
      latex-build-engine 'xetex
      latex-enable-auto-fill nil
      latex-view-pdf-in-split-window t
      latex-enable-folding t
      latex-refresh-preview t
      latex-enable-magic t
      latex-view-with-pdf-tools t
      latex-view-pdf-in-split-window t
      magic-latex-enable-block-align t
      magic-latex-enable-suscript nil
      magic-latex-enable-inline-image t)
     pdf
     epub
     (c-c++
      :variables
      c-c++-enable-google-style t
      c-c++-enable-google-newline t
      )
     (cmake
      :variables
      ;; cmake-backend 'lsp
      cmake-enable-cmake-ide-support t)
     (shell
      :variables
      shell-default-shell 'multi-vterm
      shell-default-position 'bottom
      shell-default-height 40
      shell-default-full-span t
      shell-default-term-shell "/bin/bash"
      shell-enable-smart-eshell t
      multi-term-program "/bin/bash"
      close-window-with-terminal t)
      shell-scripts
      docker
     (xclipboard
      :variables
      xclipboard-enable-cliphist nil)
     (org
      :variables
      org-enable-notifications t
      org-enable-org-contacts-support t
      org-enable-epub-support t
      org-enable-verb-support t
      org-enable-roam-support t
      org-enable-roam-protocol t
      org-enable-roam-ui t
      org-enable-transclusion-support t
      org-enable-modern-support t)
     tmux
     yaml
     prettier
     (json
      :variables
      json-fmt-tool 'prettier)
     eww
     search-engine
     systemd
     typography

     ;; -- private layers -----------------------------------------------------

     ;; ---- extra config for official layers

     all-the-icons-extra
     chinese-extra
     compleseus-extra
     emacs-lisp-extra
     latex-extra
     org-extra
     shell-extra
     tmux-extra

     ;; ---- overridden official layers

     emoji
     python

     ;; ---- created config layers

     emacs-extra
     browsers
     demo
     lazycat
     lsp-bridge
     media
     treesit ;; emacs29 native package
     ui
     workspace

     ;; ---- disabled layers

     ;; eaf
     ;; eaf-extra
     ;; treemacs-extra
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(vi-tilde-fringe

     ;; -- [org] layer ---------------------------------------------------------

     org-projectile org-jira ox-jira org-trello org-brain org-journal
     org-asciidoc org-superstar org-sudoku org-screen org-re-reveal
     org-rich-yank ;; org-bullets

     ;; -- [spacemacs-layouts] layer -------------------------------------------

     ;; window-purpose ;; conflict with `org-transclusion' live-sync edit
     ;; spaceline
     helm ivy
     persp-mode  ;; use `eyebrowser' to manage workspaces
     eyebrowse   ;; use `burly.el' to bookmark workspaces
     counsel-projectile

     ;; -- [unicode-fonts] layer -----------------------------------------------

     unicode-fonts

     ;; -- [dotfile] layer -----------------------------------------------------

     persistent-soft

     ;; -- [chinese] layer -----------------------------------------------------

     ;; NOTE: I use `rime' IM
     chinese-wbim fcitx pyim pyim-basedict pyim-wbdict

     ;; -- [spacemacs], [spacemacs-evil] layer ---------------------------------

     ;; NOTE: I don't use `evil'

     ;; spaceline holy-mode
     ;; evil ;; required by the spacemacs modeline
     ;; evil-evcilified-state ;; required by the spacemacs modeline
     evil-cleverparens ;; required by emacs-lisp layer
     evil-lisp-state   ;; required by emacs-lisp layer
     evil-tex evil-visualstar evil-visual-mark-mode evil-unimpaired
     evil-tutor evil-textobj-line evil-surround evil-org evil-numbers
     evil-nerd-commenter evil-matchit evil-lion evil-indent-plus
     evil-iedit-state evil-goggles evil-exchange evil-escape
     evil-ediff evil-collection evil-args evil-anzu

     ;; -- [auto-complete] layer ------------------------------------------------

     ;; NOTE: I don't use `helm', `company', nor `counsel' completion systems
     helm company company-lua company-emoji
     counsel counsel-gtags swiper

     ;; -- [syntax-checking] layer -----------------------------------------------

     flycheck-pos-tip

     ;; -- [colors] layer --------------------------------------------------------

     ;; NOTE: I don't need too many colors
     color-identifiers-mode rainbow-mode rainbow-identifiers

     ;; -- [spacemacs-navigation] layer ------------------------------------------

     ace-link ;; winum

     ;; -- Packages that are exclueded for tests ----------------------------

     ;; typo-suggest
     ;; undo-tree
     ;; volatile-highlights
     )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused
   ;; dotspacemacs-install-packages 'used-only
   ))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   ;; dotspacemacs-emacs-pdumper-executable-file "spacemacs"

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
   dotspacemacs-elpa-timeout 15

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; file-name-handler-alist nil

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
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
   ;;dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   ;; dotspacemacs-startup-banner '100
   dotspacemacs-startup-banner nil

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

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
   dotspacemacs-startup-lists '(;; (bookmarks . 10)
                                (recents . 15)
                                (projects . 10))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'nil

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'fundamental-mode

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
   dotspacemacs-themes '(
                         spacemacs-dark
                         spacemacs-light
                         doom-manegarm
                         github-dark-vscode
                         modus-vivendi
                         modus-operandi
                        )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator nil :separator-scale 0.8)
   ;; dotspacemacs-mode-line-theme '(all-the-icons :separator arrow)
   ;; dotspacemacs-mode-line-theme 'doom
   ;; dotspacemacs-mode-line-theme 'vanilla

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Sarasa Mono SC Nerd Font"
                               :size 12.0
                               :powerline-scale 1.0
                               ;; :weight bold
                               ;; :width normal
                               )

   ;; The leader key (default "SPC")
   ;; dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   ;; dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   ;; dotspacemacs-ex-command-key ":"

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
   dotspacemacs-display-default-layout t

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
   dotspacemacs-enable-paste-transient-state t

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
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 80

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

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
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
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
   dotspacemacs-line-numbers '(:relative nil
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
   ;; NOTE: I use systemd to start emacs server
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
   dotspacemacs-frame-title-format "%a@%t|%U@%S"
   ;; dotspacemacs-frame-title-format "%U@%S"
   ;; dotspacemacs-frame-title-format nil

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
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
  (setenv "DICTIONARY" "en_US")

  ;; set time locale to standard format, avoid chinese time stamps in org mode.
  (setq system-time-locale "C") ;; also can be solved by (setenv "LC_ALL" "C")

  ;; (setenv "XDG_RUNTIME_DIR" (format "/run/user/%d" (user-uid)))

  (setq proxy-server-ip "192.168.0.23")
  (setq proxy-server-port "7890")

  ;; NOTE: List of proxy servers
  ;;   - Local proxy: 127.0.0.1
  ;;   - VM virtual LAN: 192.168.122.1
  ;;   - Home LAN 1:  192.168.0.23
  ;;   - Home LAN 2:  192.168.2.2

  (setenv "all_proxy" (concat "socks5://"
                              proxy-server-ip ":"
                              proxy-server-port))
  (setenv "http_proxy" (concat "http://"
                               proxy-server-ip ":"
                               proxy-server-port))
  (setenv "https_proxy" (concat "https://"
                                proxy-server-ip ":"
                                proxy-server-port))
  )


(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; get rid of "Warning: Package cl is deprecated" and obsoleted package messages
  (setq byte-compile-warnings '((not cl-functions)))

  ;; package.el
  ;; ;; bfsu mirrors
  ;; (setq configuration-layer-elpa-archives
  ;;       '(("melpa-cn" . "http://mirrors.bfsu.edu.cn/elpa/melpa/")
  ;;         ("org-cn" . "http://mirrors.bfsu.edu.cn/elpa/org/")
  ;;         ("gnu-cn" . "http://mirrors.bfsu.edu.cn/elpa/gnu/")
  ;;         ("non-gnu" . "https://elpa.nongnu.org/nongnu/")))
  ;; ;; tuna mirrors
  ;; (setq configuration-layer-elpa-archives
  ;;       `(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ;;         ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
  ;;         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
  ;;         ("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;;         ;; ("sunrise-commander"  .  "https://mirrors.tuna.tsinghua.edu.cn/elpa/sunrise-commander/")
  ;;         ))

  ;; FIXME: new frame will not use C locale
  ;; set time locale to standard format, avoid chinese time stamps in org mode.
  (setq system-time-locale "C") ;; also can be solved by (setenv "LC_ALL" "C")

  (setq user-full-name "Xin Yang"
        user-mail-address "xin2.yang@Gail.com")
  (setq warning-minimum-level :error) ;; was :emergency, disable common warnings
  (setq max-lisp-eval-depth 10000)  ;; increase eval depth
  (setq auto-window-vscroll nil)    ;; reduce function calls
  (setq frame-resize-pixelwise t)

  ;; load custom-file
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file) (load custom-file))

  ;; load kbd macros
  (setq macro-file (concat user-emacs-directory "macros.el"))
  (when (file-exists-p macro-file) (load macro-file))
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  ;;(spacemacs/dump-modes '(org-mode))
)


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer

Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; -- vanilla Emacs extra config -------------------------------------------

  ;; Automatically update timestamp of files
  (setq time-stamp-start "Time-stamp:"
        time-stamp-end "\n"
        time-stamp-format " <%Y-%02m-%02d %3a %02H:%02M by %u on %s>"
        time-stamp-time-zone t)
  (add-hook 'write-file-hooks #'time-stamp)

  ;; set time locale to standard format, avoid chinese time stamps in org mode.
  (setq system-time-locale "C") ;; also can be solved by (setenv "LC_ALL" "C")

  ;; change line spacing
  (setq-default line-spacing nil)

  ;; Fixed frame size

  ;; For font size 16
  ;; (add-list-to-list 'default-frame-alist '((height . 18) (width . 82)))
  ;; (add-list-to-list 'initial-frame-alist '((height . 18) (width . 82)))

  ;; For font size 14
  ;; (add-list-to-list 'default-frame-alist '((height . 21) (width . 90)))
  ;; (add-list-to-list 'initial-frame-alist '((height . 21) (width . 90)))

  ;; For font size 12
  (add-list-to-list 'default-frame-alist '((height . 25) (width . 115)))
  (add-list-to-list 'initial-frame-alist '((height . 25) (width . 115)))

  ;; NOTE: resize frame is not a good idea, your cursor would move out of the
  ;; boundaries
  ;; (set-frame-height frame 25)
  ;; (set-frame-width frame 100)

  ;; Enable background transparency
  ;; (spacemacs/enable-background-transparency)

  ;; enable modeline display time
  ;; (spacemacs/toggle-display-time-on)

  ;; prevent emacs auto resizing frame size
  (setq-default frame-inhibit-implied-resize t)

  ;; `simple' package, visual-line-mode
  (setq-default visual-line-fringe-indicators '(left-curly-arrow nil))

  ;; `epa' package, EasyPG encryption and decryption
  (setq epa-file-select-keys nil ;; don't ask for key
        epa-pinentry-mode 'loopback) ;; Allow epa password input in minibuffer.

  ;; Registers, "(emacs)Text Registers"
  (setq register-separator ?+)
  ;; (setq register-preview-delay 0)
  (set-register register-separator
                "\n✂~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  ;; Add more register keys
  (global-set-key (kbd "C-x r a") 'append-to-register)
  (global-set-key (kbd "C-x r p") 'prepend-to-register)
  (global-set-key (kbd "C-x r v") 'view-register)

  ;; Bookmarks "(emacs)Bookmarks"
  (global-set-key (kbd "C-x r C-s") 'bookmark-save)

  ;; -- spacemacs official layers extra config ---------------------------------

  ;; `spacemacs-bootstrap' layer

  ;;;; `async.el' package
  (defun spacemacs-bootstrap/post-init-async ()
    (dired-async-mode 1)
    (async-bytecomp-package-mode 1)
    (setq message-send-mail-function 'async-smtpmail-send-it))

  ;; `spacemacs-defaults' layer

  ;;;; `hl-line' package
  ;;;;
  ;;;; It makes the display of the current line clearer, especially in a frame
  ;;;; with transparent background.
  (spacemacs/toggle-highlight-current-line-globally-on)

  ;; `spacemacs-navigation' layer

  ;;;; `ace-window' package
  ;;;;
  ;;;; NOTE: `ace-window' functions mess up with `eaf' buffers

  (global-set-key (kbd "C-x 0") 'ace-delete-window)
  (global-set-key (kbd "C-x o") 'ace-select-window)
  (global-set-key (kbd "C-x w d") 'ace-delete-window)
  (global-set-key (kbd "C-x w o") 'ace-select-window)

  ;; `spacemacs-editing' layer
  ;; FIXME:
  ;;
  ;;  Error (use-package): password-generator/:init: Key sequence i p 1 starts
  ;;  with non-prefix key i p
  ;; (spacemacs/declare-prefix "ip" "passwords")

  ;; `spacemacs-editing-visual' layer

  ;; `spell-checking' layer

  ;;;; `ispell' package
  (defun spell-checking/post-init-ispell ()
    ;; aspell works great, but hunspell is more accurate.
    ;; ispell-program-name "aspell"
    ;; ispell-dictionary "american"
    (setq ispell-program-name "hunspell")

    ;; `ispell-set-spellchecker-params' has to be called
    ;; before `ispell-hunspell-add-multi-dic' will work
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,en_GB")
    (setq ispell-dictionary "en_US,en_GB")
    (ispell-change-dictionary "en_US" t))

  ;;;; `flyspell' package
  (spacemacs/add-to-hooks 'flyspell-mode
                          '(org-mode-hook markdown-mode-hook))

  ;; `multiple-cursors' layer

  ;;;; `multiple-cursors' package
  (spacemacs/set-leader-keys
    "smA" 'mc/edit-beginnings-of-lines
    "smE" 'mc/edit-ends-of-lines)

  ;; `search-engine' layer
  (spacemacs/declare-prefix "aws" "search")
  (spacemacs/set-leader-keys
    "aw/"  nil
    "awsa" 'engine/search-amazon
    "awsb" 'engine/search-bing
    "awsd" 'engine/search-docker-hub
    "awsD" 'engine/search-duck-duck-go
    "awsh" 'engine/search-github
    "awsg" 'engine/search-google
    "awsi" 'engine/search-google-images
    "awsm" 'engine/search-google-maps
    "awst" 'engine/search-twitter
    "awsG" 'engine/search-project-gutenberg
    "awsy" 'engine/search-youtube
    "awss" 'engine/search-stack-overflow
    "awsw" 'engine/search-wikipedia
    "awsp" 'engine/search-pip
    "awsP" 'engine/search-python-doc
    "awsc" 'engine/search-c++-api-reference
    "awsu" 'engine/search-ubuntu-packages
    "awse" 'engine/search-melpa
    "awsl" 'engine/search-ctan
    )

  ;; `typographic' layer, `typo.el' package

  ;;  NOTE: I found I did not use typographic punctuations a lot.

  ;; (spacemacs/add-to-hooks 'spacemacs/toggle-typographic-substitutions-on
  ;;                         '(org-mode-hook text-mode-hook markdown-mode-hook))

  ;; `git' layer, `magit' package

  ;; Magit requires ‘transient’ >= 0.5.0, but due to bad defaults, Emacs’
  ;; package manager, refuses to upgrade this and other built-in packages to
  ;; higher releases from GNU Elpa.
  ;;
  ;; To fix this, you have to add this to your init file:
  (setq package-install-upgrade-built-in t)

  ;; Then evaluate that expression by placing the cursor after it
  ;; and typing C-x C-e.
  ;;
  ;; Once you have done that, you have to explicitly upgrade ‘transient’:
  ;;
  ;; { M-x package-install RET transient RET }
  ;;
  ;; Then you also must make sure the updated version is loaded,
  ;; by evaluating this form:
  ;;
  ;; (progn (unload-feature 'transient t) (require 'transient))
  ;;
  ;; If this does not work, then try uninstalling Magit and all of its
  ;; dependencies.  After that exit and restart Emacs, and only then
  ;; reinstalling Magit.

  ;; Add toggle for `git-timemachine'
  (spacemacs/set-leader-keys "gT" 'git-timemachine-toggle)


  ;; -- My own layers and packages extra config --------------------------------
  ;; (xy/set-fonts)
  (xy/set-emoji-font)
  (xy/org-roam-dailies-create 1)
  ;; (xy/workspace-restore)

  ;; -- Dynamic emacs config that depends on the work environment ------------

  ;; Make `xy/adapt-emacs-config' been triggered once after a new frame is made.
  ;; In other cases, I can run it manually.
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook 'xy/adapt-emacs-config)
    ;; (add-hook 'after-make-frame-functions 'xy/adapt-emacs-config))
    (add-hook 'window-setup-hook 'xy/adapt-emacs-config))

  ;; (add-hook 'kill-emacs-hook #'xy/workspace-save)
  ;; (add-hook 'server-done-hook #'xy/workspace-save)
  ;; (add-to-list 'delete-frame-functions #'xy/workspace-save)
  ;; (delq nil (delete-dups delete-frame-functions))

  ;; (spacemacs/set-leader-keys "Te" 'xy/adapt-emacs-config)
  (global-set-key (kbd "<f12>") 'xy/adapt-emacs-config)
  )

(defun xy/adapt-emacs-config (&optional frame)
  "Adapt emacs config for different environments."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    ;; (set-frame-parameter frame 'name
    ;;                      (concat user-login-name "@" system-name))
    (xy/adapt-lsp-bridge-config frame)
    (xy/adapt-vertico-posframe-config frame)
    (xy/adapt-org-config frame)
    (xy/adapt-ui-config frame)))

;; file ends here
