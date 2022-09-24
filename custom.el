(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-default-style 'libnotify)
 '(bibtex-autokey-name-year-separator "-")
 '(bibtex-autokey-titleword-separator "-")
 '(bibtex-autokey-titlewords 2)
 '(bibtex-autokey-titlewords-stretch 1)
 '(bibtex-autokey-year-length 4)
 '(bibtex-autokey-year-title-separator "-")
 '(bibtex-completion-additional-search-fields '(keywords))
 '(bibtex-completion-bibliography '("~/org/bib/all.bib"))
 '(bibtex-completion-display-formats
   '((article . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
     (inbook . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
     (incollection . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (t . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))
 '(bibtex-completion-library-path '("~/doc/"))
 '(bibtex-completion-notes-path "~/org/roam/")
 '(bibtex-completion-notes-template-multiple-files
   "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: 

See [[cite:&${=key=}]]
")
 '(bibtex-completion-pdf-open-function
   '(closure
     (t)
     (fpath)
     (call-process "open" nil 0 nil fpath)))
 '(company-emoji-insert-unicode t)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "dad40020beea412623b04507a4c185079bff4dcea20a93d8f8451acb6afc8358" "a0415d8fc6aeec455376f0cbcc1bee5f8c408295d1c2b9a1336db6947b89dd98" "d600c677f1777c1e4bfb066529b5b73c0179d0499dd4ffa3f599a0fb0cfbd501" default))
 '(global-hardhat-mode t)
 '(hardhat-basename-protected-regexps
   '("~\\'" "\\.lock\\'" "\\.ix\\'" "\\`test\\.out\\'" "-autoloads\\.el\\'" "\\`Desktop\\.ini\\'" "\\`META\\.yml\\'" "\\`MYMETA\\.yml\\'" "\\`TAGS\\'" "\\`Thumbs\\.db\\'" "\\`\\.dropbox\\'" "\\`\\.dropbox\\.cache\\'" "\\`\\.emacs\\.desktop\\'" "\\`\\.emacs\\.desktop\\.lock\\'" "\\.orig\\'" "\\.rej\\'" "\\.bak\\'"))
 '(hardhat-fullpath-protected-regexps
   '("~/\\.emacs\\.d/elpa/" "~/\\.cpan/" "~/\\.cabal/" "~/perl5/perlbrew/" "~/\\.npm/" "~/\\.virtualenv/" "~/\\.virthualenv/" "~/\\.rvm/" "/[._]build/" "/\\.bzr/" "/\\.coverage/" "/\\.git/" "/\\.hg/" "/\\.rspec/" "/\\.sass-cache/" "/\\.svn/" "/_MTN/" "/_darcs/" "/CVS/" "/pm_to_blib/" "/RCS/" "/SCCS/" "/blib/" "/test_output/" "~/\\.emacs\\.d/\\.cask/" "~/\\.cask/" "~/\\.cargo/" "~/\\.conda/" "~/\\.docker/" "~/\\.local/" "~/\\.rustup/" "~/\\.ssh/" "~/bin/"))
 '(ispell-program-name "hunspell")
 '(lsp-keymap-prefix "M-#")
 '(modus-themes-bold-constructs t)
 '(modus-themes-diffs 'desaturated)
 '(modus-themes-fringes 'subtle)
 '(modus-themes-headings
   '((1 background overline rainbow)
     (2 background rainbow)
     (3 no-bold rainbow)
     (t no-bold)))
 '(modus-themes-italic-constructs t)
 '(modus-themes-mode-line '(nil accented))
 '(modus-themes-org-agenda '((header-block variable-pitch) (header-date bold-today)))
 '(modus-themes-org-blocks nil)
 '(modus-themes-paren-match '(bold intense))
 '(modus-themes-prompts '(intense bold))
 '(modus-themes-region '(bg-only))
 '(modus-themes-scale-headings t)
 '(modus-themes-subtle-line-numbers t)
 '(modus-themes-syntax '(yellow-comments green-strings))
 '(modus-themes-tabs-accented t)
 '(modus-themes-variable-pitch-headings t)
 '(modus-themes-variable-pitch-ui t)
 '(org-after-todo-state-change-hook
   '((lambda nil
       (when
           (equal org-state "DONE")
         (xy/org-roam-copy-todo-to-today)))
     (closure
      (t)
      nil
      (if
          (or
           (string= org-state "SOMEDAY")
           (string= org-state "TODO"))
          (org-remove-timestamp-with-keyword org-scheduled-string))
      (if
          (string= org-state "NEXT")
          (org-deadline nil "+0"))
      (if
          (string= org-state "DONE")
          (alert "WELL DONE" :title "Agenda" :category 'Emacs :severity 'trivial))
      (if
          (string= org-state "REVIEW")
          (org-fc-type-double-init)))))
 '(org-agenda-block-separator 9473)
 '(org-agenda-custom-commands
   '(("d" "Day Planner"
      ((agenda ""
               ((org-agenda-span 1)
                (org-agenda-deadline-warning-days 14)
                (org-agenda-use-time-grid t)
                (org-agenda-skip-scheduled-if-done t)
                (org-agenda-skip-deadline-if-done t)
                (org-agenda-skip-timestamp-if-done t)
                (org-agenda-skip-archived-trees t)
                (org-agenda-skip-comment-trees t)
                (org-agenda-todo-list-sublevel t)
                (org-agenda-timeline-show-empty-dates t)))
       (tags-todo "TODO<>\"NEW\"+TODO<>\"TODO\"+TODO<>\"SOMEDAY\"-SCHEDULED<=\"<+7d>\"-SCHEDULED>\"<+14d>\"-DEADLINE<=\"<+7d>\"-DEADLINE>\"<+14d>\"-repeat-appt-fc"
                  ((org-agenda-overriding-header "Pending Next Actions")
                   (org-tags-match-list-sublevels t)))
       (tags-todo "TODO=\"TODO\"-SCHEDULED-DEADLINE-repeat"
                  ((org-agenda-overriding-header "Task Inbox")
                   (org-tags-match-list-sublevels t)))
       (tags-todo "TODO=\"NEW\""
                  ((org-agenda-overriding-header "New Stuff")
                   (org-tags-match-list-sublevels t)))
       (tags-todo "SCHEDULED>=\"<+1d>\"+SCHEDULED<=\"<+7d>\"-repeat-fc"
                  ((org-agenda-overriding-header "Scheduled Tasks in 7 Days")
                   (org-tags-match-list-sublevels nil)))
       (tags-todo "TODO=\"SOMEDAY\""
                  ((org-agenda-overriding-header "Future Work")
                   (org-tags-match-list-sublevels nil))))
      nil)))
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-exporter-settings
   '((ps-number-of-columns 2)
     (ps-landscape-mode t)
     (org-agenda-add-entry-text-maxlines 5)
     (htmlize-output-type 'css)))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   '((agenda time-up category-keep priority-down todo-state-up)
     (todo time-up category-keep priority-down todo-state-up)
     (tags time-up category-keep priority-down todo-state-up)
     (search time-up category-keep priority-down todo-state-up)))
 '(org-agenda-todo-ignore-scheduled 'all)
 '(org-agenda-todo-list-sublevels nil)
 '(org-agenda-window-frame-fractions '(0.2 . 0.8))
 '(org-agenda-window-setup 'only-window)
 '(org-appear-autoentities t)
 '(org-appear-autolinks 'just-brackets t)
 '(org-appear-autosubmarkers t t)
 '(org-appear-delay 0.8)
 '(org-appear-inside-latex t)
 '(org-archive-save-context-info '(time file category todo priority itags olpath ltags))
 '(org-attach-archive-delete 'query)
 '(org-attach-id-dir "data/")
 '(org-attach-store-link-p 'attached)
 '(org-capture-templates
   '(("t" "Task" entry
      (file+headline "~/org/roam/inbox.org" "Tasks")
      "** TODO %^{Task}
:LOGBOOK:
- Create time: %U
- From: %a
:END:
- Tags:     [ add exsisting reference notes as tags]
- See also: [ add existing literate, fleeting, and permanent notes that relates with this project ]
" :prepend t :empty-lines 1 :clock-keep t)
     ("n" "Note" entry
      (file+headline "~/org/roam/inbox.org" "Notes")
      "** NEW %^{Title}
:LOGBOOK:
- Create time: %U
- From: %a
:END:
- Tags:     [ add exsisting reference notes as tags]
- See also: [ add existing literate, fleeting, and permanent notes that relates with this project ]
" :prepend t :empty-lines 1 :clock-keep t)
     ("e" "English" entry
      (file+headline "~/org/roam/inbox.org" "English")
      "** NEW %?
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:
:LOGBOOK:
- Create time: %U
- From: %a
:END:" :prepend t :empty-lines 1 :clock-keep t)
     ("b" "Bookmark" entry
      (file+headline "~/org/roam/inbox.org" "Bookmark")
      "** NEW %a
:PROPERTIES:
:SCORE: %?
:END:
:LOGBOOK:
- Create time: %U
- From: %a
:END:
- URL: %L
- Tags: [add existing reference nodes as tags]
- Description:" :prepend t :empty-lines 1 :clock-keep t)
     ("c" "Contacts" entry
      (file "~/org/roam/contacts.org.gpg")
      "* %(org-contacts-template-name)
:PROPERTIES:
:COMPANY:
:POSITION:
:OCCUPATION:
:NOTE:
:PHONE:
:WeChat:
:WXWORK:
:EMAIL: %(org-contacts-template-email)
:ALITINGTING:
:QQ:
:ALIAS:
:NICKNAME:
:BIRTHDAY:
:ADDRESS:
:END:" :prepend t :empty-lines 1 :clock-keep t)
     ("x" "Password" entry
      (file "~/org/roam/passwords.org.gpg")
      "* %?
:LOGBOOK:
- Create time: %U
- From: %a
:END:
- Website:
- Username:
- Password:
- Tags: [add existing reference nodes as tags]
- Description:" :prepend t :empty-lines 1 :clock-keep t)))
 '(org-clock-history-length 10)
 '(org-clock-idle-time 15)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-switch-to-state "WAITING")
 '(org-clock-persist t)
 '(org-clock-persist-query-save t)
 '(org-clock-report-include-clocking-task t)
 '(org-clock-sound t)
 '(org-columns-default-format
   "%CATEGORY(Cat.) %PRIORITY(Pri.) %6TODO(State) %35ITEM(Details) %ALLTAGS(Tags) %5NUM_POMODORO(Plan){:} %6CLOCKSUM(Clock){Total} %SCORE(SCORE)")
 '(org-confirm-babel-evaluate nil)
 '(org-contacts-files '("~/org/roam/contacts.org.gpg"))
 '(org-crypt-disable-auto-save 'encrypt)
 '(org-crypt-key nil)
 '(org-default-notes-file "~/org/notes.org")
 '(org-directory "~/org")
 '(org-ditaa-eps-jar-path "/opt/DitaaEps/DitaaEps.jar")
 '(org-ditaa-jar-path "/opt/ditaa/ditaa.jar")
 '(org-download-edit-cmd "krita %s")
 '(org-download-image-org-width 200)
 '(org-download-method 'attach)
 '(org-download-screenshot-method "scrot -s %s")
 '(org-edit-src-turn-on-auto-save t)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii beamer html latex man md odt org texinfo))
 '(org-export-use-babel nil)
 '(org-export-with-sub-superscripts '{})
 '(org-global-properties
   '(("POMODORO_ALL" . "0 1 2 3 4 5")
     ("SCORE_ALL" . "0 1 2 3 4 5")))
 '(org-indirect-buffer-display 'current-window)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-log-redeadline 'note)
 '(org-log-refile 'time)
 '(org-log-reschedule 'time)
 '(org-log-state-notes-insert-after-drawers t)
 '(org-modules
   '(ol-bbdb ol-bibtex org-crypt ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe org-mouse org-protocol ol-rmail ol-w3m ol-elisp-symbol ol-git-link ol-man org-toc))
 '(org-plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\""))
 '(org-plantuml-jar-path "/opt/plantuml/plantuml.jar" t)
 '(org-refile-targets '((nil :maxlevel . 4) (org-agenda-files :maxlevel . 4)))
 '(org-refile-use-outline-path 'file)
 '(org-reverse-note-order t)
 '(org-roam-capture-ref-templates
   '(("a" "annote" entry "** %?
:LOGBOOK:
- Create time: %U
:END:

${body}" :target
(file+head "${slug}.org" "#+title: ${title}
#+filetags: literature
- Tags:     [ add exsisting reference notes as tags]
- See also: [ add existing literate, fleeting, and permanent notes that relates with this project ]

* Abstract

* Local backup
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:")
:immediate-finish t :jump-to-captured t :empty-lines 1)))
 '(org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
- Tags: [add existing reference nodes as tags]
")
      :prepend t :empty-lines 1 :unnarrowed t)
     ("p" "project" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+category: ${title}
#+filetags: PROJECT
- Areas:    [ add area-of-responsibility nodes ]
- Tags:     [ add exsisting reference notes as tags]
- See also: [ add existing literate, fleeting, and permanent notes that relates with this project ]

* Goal

[ Describe WHAT YOU REALLY WANT if the project was completed ]

* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:

** TODO Define the goal of the project.
** TODO Add areas-of-responsibility, tags, and exsisting notes related to this project.
** TODO Set a project deadline.
** Version 1.0
*** TODO Finish v1.0
")
      :prepend t :empty-lines 1 :clock-keep t :unnarrowed t)
     ("s" "software" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+filetags: reference
- Tags:     [ add exsisting reference notes as tags]
- See also: [ add existing literate, fleeting, and permanent notes that relates with this project ]

* About ${title}

[ Logo image ]
[ TL;DR - basic information about ${title} ]

* Resources

* Concepts

** Terminology

** My understandings

* Installation

* Usage

** Basic usage

** Configuration

** Use cases

** Tricks & Tips

")
      :prepend t :empty-lines 1 :clock-keep t :unnarrowed t)
     ("l" "literate programming" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+filetags: code
#+auto_tangle: t
#+PROPERTY: header-args:lang :tangle  " /path/to/tangled_source_code "
- Project:  [ add parent project note ]
- Tags:     [ add exsisting reference notes as tags ]
- See also: [ add existing literate, fleeting, and permanent notes that relates with this project ]

* Description of ${title}

[ TL;DR - basic description about %? ]

* Design

* Main code

* Unit test

* References

")
      :prepend t :empty-lines 1 :clock-keep t :unnarrowed t)))
 '(org-roam-completion-everywhere t)
 '(org-roam-dailies-capture-templates
   '(("d" "default" entry "* %U %?" :target
      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:")
      :prepend t :empty-lines 1 :unnarrowed t)))
 '(org-roam-dailies-directory "~/org/dailies")
 '(org-roam-db-autosync-mode t)
 '(org-roam-db-location "~/org/org-roam.db")
 '(org-roam-directory "~/org/roam")
 '(org-roam-list-files-commands '(rg))
 '(org-roam-mode-sections
   '(org-roam-backlinks-section org-roam-reflinks-section org-roam-unlinked-references-section))
 '(org-roam-protocol-store-links t)
 '(org-src-ask-before-returning-to-edit-buffer nil)
 '(org-src-preserve-indentation t)
 '(org-stuck-projects '("+PROJECT/-SOMEDAY-DONE" ("NEXT" "STARTED")))
 '(org-tag-persistent-alist
   '((:startgrouptag)
     ("PROJECT" . 80)
     ("ARCHIVE" . 65)
     ("CONFIDENTIAL" . 67)
     ("FLAGGED" . 70)
     ("ATTACH" . 84)
     (:endgrouptag)
     (:startgrouptag)
     ("reference" . 114)
     ("code" . 99)
     ("literature" . 108)
     ("permanent" . 112)
     ("fleeting" . 102)
     ("hub" . 104)
     (:endgrouptag)
     (:startgrouptag)
     ("noexport" . 78)
     ("TOC" . 84)
     (:endgrouptag)))
 '(org-todo-keywords
   '((sequence "TODO(t)" "SOMEDAY(x)" "NEXT(n)" "STARTED(s!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
     (sequence "NEW(a)" "REVIEW(r!)" "|" "MARK(m!)" "USELESS(u!)")))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-treat-insert-todo-heading-as-state-change t)
 '(org-use-property-inheritance "header-args\\|shebang\\|session\\|DIR\\|dir")
 '(org-use-tag-inheritance nil)
 '(org-wild-notifier-alert-time '(25 15 10 5 3 1))
 '(org-wild-notifier-keyword-blacklist '("DONE" "CANCELLED" "MARK" "USELESS"))
 '(org-wild-notifier-keyword-whitelist nil)
 '(org-wild-notifier-tags-blacklist '("ARCHIVE"))
 '(package-selected-packages
   '(evil-evilified-state zoom-window zonokai-emacs zenburn-theme zen-and-art-theme youdao-dictionary yasnippet-snippets yapfify yaml-mode xwwp-follow-link-helm xterm-color ws-butler writeroom-mode winum white-sand-theme which-key wgrep web-mode web-beautify volatile-highlights vim-powerline vi-tilde-fringe verb valign uuidgen use-package unkillable-scratch unfill undo-tree underwater-theme ujelly-theme typo-suggest twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-all-the-icons toxi-theme toml-mode toc-org tern terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit systemd symon symbol-overlay sunny-day-theme sublime-themes subed subatomic256-theme subatomic-theme string-edit stickyfunc-enhance srefactor sql-indent sphinx-doc spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slime-company slim-mode shfmt shell-pop seti-theme selectrum seeing-is-believing scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode ron-mode robe rime reverse-theme restart-emacs rebecca-theme rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme racer quickrun pytest pyim-basedict pyim pyenv-mode pydoc py-isort purple-haze-theme pug-mode professional-theme prettier-js popwin poetry plantuml-mode planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persistent-scratch pdf-view-restore password-generator paradox pangu-spacing ox-gfm ox-epub overseer orgit-forge organic-green-theme org-wild-notifier org-web-tools org-vcard org-tree-slide org-transclusion org-superstar org-sticky-header org-roam-ui org-roam-bibtex org-rich-yank org-ref org-projectile org-present org-pomodoro org-noter-pdftools org-mime org-fragtog org-fc org-emms org-download org-contrib org-contacts org-cliplink org-bullets org-auto-tangle org-appear orderless open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-tmux ob-ipython ob-async npm-mode nov nose nodejs-repl noctilux-theme no-littering naquadah-theme nameless mwim mustang-theme multi-vterm multi-term multi-line monokai-theme monochrome-theme molokai-theme moe-theme modus-themes mmm-mode minitest minimal-theme material-theme markdown-toc marginalia majapahit-theme magit-gitflow magic-latex-buffer madhat2r-theme lush-theme lsp-ui lsp-python-ms lsp-pyright lsp-origami lsp-latex lsp-bridge lorem-ipsum livid-mode live-py-mode link-hint light-soap-theme ligature kaolin-themes jupyter json-reformat json-navigator json-mode js2-refactor js-doc journalctl-mode jbeans-theme jazz-theme ivy-yasnippet ivy-rich ivy-prescient ir-black-theme inspector insert-shebang inkpot-theme info+ indent-guide importmagic import-js impatient-mode ibuffer-projectile hybrid-mode hungry-delete holy-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-icons helm-gtags helm-git-grep helm-flx helm-descbinds helm-ctest helm-css-scss helm-company helm-cider helm-c-yasnippet helm-bibtex helm-ag hc-zenburn-theme hardhat gruvbox-theme gruber-darker-theme graphviz-dot-mode grandshell-theme gotham-theme google-translate google-c-style golden-ratio gnuplot gitignore-templates git-timemachine git-modes git-messenger git-link git-gutter-fringe+ gif-screencast gh-md ggtags gendoxy gandalf-theme fuzzy font-lock+ flyspell-popup flyspell-correct-popup flyspell-correct-helm flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-elsa flycheck-clj-kondo flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode find-by-pinyin-dired farmhouse-theme fancy-narrow fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-tex evil-terminal-cursor-changer evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-collection evil-cleverparens evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help enh-ruby-mode engine-mode emr emojify emoji-cheat-sheet-plus emms-info-mediainfo emmet-mode embark-consult emamux elisp-slime-nav elisp-def ein editorconfig ebib eaf dumb-jump drag-stuff dracula-theme dotenv-mode doom-themes doom-modeline dockerfile-mode docker django-theme disaster dired-quick-sort diminish diff-hl devdocs demo-it define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dap-mode dakrone-theme cython-mode cyberpunk-theme csv-mode cpp-auto-include counsel-projectile counsel-gtags consult-yasnippet consult-lsp conda compleseus-spacemacs-help company-ycmd company-web company-statistics company-shell company-rtags company-reftex company-quickhelp company-plsense company-math company-emoji company-c-headers company-box company-auctex company-anaconda common-lisp-snippets command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode code-cells cmake-mode cmake-ide clues-theme clojure-snippets clean-aindent-mode cider-eval-sexp-fu chruby chocolate-theme chinese-conv cherry-blossom-theme centered-cursor-mode ccls cask cargo busybee-theme bundler bubbleberry-theme browse-at-remote blacken birds-of-paradise-plus-theme badwolf-theme aweshell auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme all-the-icons-ibuffer all-the-icons-dired alect-themes aggressive-indent afternoon-theme adoc-mode add-node-modules-path ace-pinyin ace-link ace-jump-helm-line ac-ispell))
 '(plantuml-indent-level 4)
 '(time-stamp-format " <%Y-%02m-%02d %3a %02H:%02M by %u on %s>")
 '(time-stamp-time-zone t)
 '(toc-org-hrefify-default "org")
 '(valign-fancy-bar t))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
