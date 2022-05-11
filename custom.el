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
 '(custom-safe-themes
   '("d600c677f1777c1e4bfb066529b5b73c0179d0499dd4ffa3f599a0fb0cfbd501" default))
 '(global-hardhat-mode t)
 '(hardhat-basename-protected-regexps
   '("~\\'" "\\.lock\\'" "\\.ix\\'" "\\`test\\.out\\'" "-autoloads\\.el\\'" "\\`Desktop\\.ini\\'" "\\`META\\.yml\\'" "\\`MYMETA\\.yml\\'" "\\`TAGS\\'" "\\`Thumbs\\.db\\'" "\\`\\.dropbox\\'" "\\`\\.dropbox\\.cache\\'" "\\`\\.emacs\\.desktop\\'" "\\`\\.emacs\\.desktop\\.lock\\'" "\\.orig\\'" "\\.rej\\'" "\\.bak\\'"))
 '(hardhat-fullpath-protected-regexps
   '("~/\\.emacs\\.d/elpa/" "~/\\.cpan/" "~/\\.cabal/" "~/perl5/perlbrew/" "~/\\.npm/" "~/\\.virtualenv/" "~/\\.virthualenv/" "~/\\.rvm/" "/[._]build/" "/\\.bzr/" "/\\.coverage/" "/\\.git/" "/\\.hg/" "/\\.rspec/" "/\\.sass-cache/" "/\\.svn/" "/_MTN/" "/_darcs/" "/CVS/" "/pm_to_blib/" "/RCS/" "/SCCS/" "/blib/" "/test_output/" "~/\\.emacs\\.d/\\.cask/" "~/\\.cask/" "~/\\.cargo/" "~/\\.conda/" "~/\\.docker/" "~/\\.local/" "~/\\.rustup/" "~/\\.ssh/" "~/bin/"))
 '(ispell-program-name "hunspell")
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
       (tags-todo "TODO=\"TODO\"-SCHEDULED-repeat"
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
 '(org-appear-autolinks 'just-brackets)
 '(org-appear-autosubmarkers t)
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
- Tags: [add existing reference nodes as tags]" :prepend t :empty-lines 1 :clock-keep t)
     ("n" "Note" entry
      (file+headline "~/org/roam/inbox.org" "Notes")
      "** NEW %^{Title}
:LOGBOOK:
- Create time: %U
- From: %a
:END:
- Tags: [add existing reference nodes as tags]" :prepend t :empty-lines 1 :clock-keep t)
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
 '(org-download-image-org-width 400)
 '(org-download-method 'attach)
 '(org-download-screenshot-method "scrot -s %s")
 '(org-edit-src-turn-on-auto-save t)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii beamer html latex man md odt org texinfo))
 '(org-export-use-babel nil)
 '(org-export-with-sub-superscripts '{})
 '(org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)))
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
   '(ol-bbdb ol-bibtex org-crypt ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe org-mouse org-protocol ol-rmail ol-w3m ol-elisp-symbol org-toc))
 '(org-noter-pdftools-use-org-id nil)
 '(org-plantuml-executable-args '("-headless" "-DRELATIVE_INCLUDE=\".\""))
 '(org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
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
- Tags: [add existing reference nodes as tags]

* Abstract

* Local backup
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:")
:immediate-finish t :jump-to-captured t :empty-lines 1)))
 '(org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
      :prepend t :empty-lines 1 :unnarrowed t)
     ("p" "project" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+category: ${title}
#+filetags: PROJECT
- Tags: [add existing reference nodes as tags]

* Goal

* Tasks
:PROPERTIES:
:ROAM_EXCLUDE: t
:END:

** TODO Define the goal of the project.
** TODO Add areas-of-responsibility tags.
** TODO Set a project deadline.")
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
 '(org-use-tag-inheritance nil)
 '(org-wild-notifier-alert-time '(25 15 10 5 3 1))
 '(org-wild-notifier-keyword-blacklist '("DONE" "CANCELLED" "MARK" "USELESS"))
 '(org-wild-notifier-keyword-whitelist nil)
 '(org-wild-notifier-tags-blacklist '("ARCHIVE"))
 '(package-selected-packages
   '(org-tree-slide demo-it seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe rbenv rake minitest helm-gtags ggtags enh-ruby-mode counsel-gtags counsel swiper ivy chruby bundler inf-ruby add-node-modules-path pyim org-ref modus-themes inspector info+ evil-collection doom-themes diff-hl dap-mode cider auto-highlight-symbol lsp-mode magit magit-section git-commit compat treemacs esxml font-lock+ evil zoom-window zonokai-emacs zenburn-theme zen-and-art-theme youdao-dictionary yasnippet-snippets yapfify yaml-mode xwwp-follow-link-helm xterm-color xr ws-butler writeroom-mode winum white-sand-theme which-key web-mode web-beautify volatile-highlights vi-tilde-fringe verb valign uuidgen use-package unkillable-scratch unfill undo-tree underwater-theme ujelly-theme typo-suggest twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-all-the-icons toxi-theme toml-mode toc-org terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit systemd symon symbol-overlay sunny-day-theme sublime-themes subed subatomic256-theme subatomic-theme string-edit sql-indent sphinx-doc spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme sesman scss-mode sass-mode ron-mode rime reverse-theme restart-emacs rebecca-theme rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme racer quickrun pytest pyenv-mode pydoc py-isort purple-haze-theme pug-mode professional-theme prettier-js popwin poetry plantuml-mode planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persistent-scratch pdf-view-restore pcre2el password-generator parseedn paradox pangu-spacing ox-gfm ox-epub overseer orgit-forge organic-green-theme org-wild-notifier org-web-tools org-vcard org-superstar org-sticky-header org-roam-ui org-roam-bibtex org-rich-yank org-projectile org-present org-pomodoro org-noter-pdftools org-mime org-fragtog org-fc org-emms org-download org-contrib org-cliplink org-appear open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-tmux ob-ipython ob-async nov nose noctilux-theme naquadah-theme nameless mwim mustang-theme multiple-cursors multi-vterm multi-term multi-line monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow magic-latex-buffer madhat2r-theme macrostep lush-theme lsp-ui lsp-treemacs lsp-python-ms lsp-pyright lsp-origami lsp-latex lorem-ipsum live-py-mode link-hint light-soap-theme kv kaolin-themes journalctl-mode jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide importmagic impatient-mode ibuffer-projectile hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-icons helm-git-grep helm-flx helm-descbinds helm-ctest helm-css-scss helm-company helm-cider helm-c-yasnippet helm-bibtex helm-ag hc-zenburn-theme hardhat gruvbox-theme gruber-darker-theme graphviz-dot-mode grandshell-theme goto-chg gotham-theme google-translate google-c-style golden-ratio gnuplot gitignore-templates git-timemachine git-modes git-messenger git-link gif-screencast gh-md gendoxy gandalf-theme fuzzy flyspell-popup flyspell-correct-helm flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-elsa flycheck-clj-kondo flx-ido flatui-theme flatland-theme find-by-pinyin-dired farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-terminal-cursor-changer evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help engine-mode emr emojify emoji-cheat-sheet-plus emms-info-mediainfo emmet-mode emamux elisp-slime-nav elisp-def ein editorconfig ebib dumb-jump drag-stuff dracula-theme dotenv-mode dockerfile-mode docker django-theme disaster dired-quick-sort diminish devdocs define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode cpp-auto-include conda company-ycmd company-web company-statistics company-rtags company-reftex company-quickhelp company-plsense company-math company-emoji company-c-headers company-auctex company-anaconda command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode cmake-mode cmake-ide clues-theme clojure-snippets clojure-mode clean-aindent-mode citeproc cider-eval-sexp-fu chocolate-theme chinese-conv cherry-blossom-theme cfrs centered-cursor-mode ccls cargo busybee-theme bui bubbleberry-theme browse-at-remote blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme annalist ample-zen-theme ample-theme all-the-icons-ibuffer all-the-icons-dired alect-themes aggressive-indent afternoon-theme ace-window ace-pinyin ace-link ace-jump-helm-line ac-ispell))
 '(plantuml-indent-level 4)
 '(time-stamp-format " <%Y-%02m-%02d %3a %02H:%02M by %u on %s>" t)
 '(time-stamp-time-zone t t)
 '(toc-org-hrefify-default "org")
 '(valign-fancy-bar t)
 '(warning-suppress-types '((lsp-mode) (lsp-mode) (lsp-mode))))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
