(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))

(require 'cl)

(defun centaur-setup-fonts ()
  "Setup different fonts for default, Chinese, and emoji."
  (interactive)
  (when (display-graphic-p)

    ;; Set default font
    (cl-loop for font in
             '("Cascadia Code"
               "Fira Code"
               "Iosevka"
               "Source Code Pro"
               "Consolas"
               "Jetbrains Mono"
               "Hack"
               "Menlo"
               "DejaVu Sans Mono"
               )
             when (font-installed-p font)
             return (set-face-attribute
                     'default nil
                     :family font
                     ;; ;; (elisp:height)
                     ;; ;; Changed for only Linux system, no sys/macp and sys/win32p required.
                     ;; :height (cond (sys/macp 130)
                     ;;               (sys/win32p 110)
                     ;;               (t 100))
                     :height 110 ;; Try to makes EN and CN fonts aligned
                     ))

    ;; Set font for Chinese characters
    (cl-loop for font in
             '("Adobe Fangsong Std"
               "WenQuanYi Micro Hei Mono"
               "FZDaHei-B02"
               "Microsoft Yahei"
               "Adobe Heiti Std"
               "FZXiaoBiaoSong-B05"
               "Adobe Song Std"
               "Adobe Kaiti Std"
               "LXGW WenKai Mono GB"
               )
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.25)))
                      (set-fontset-font t 'han (font-spec :family font))))

    ;; Set font for color emojis
    (cl-loop for font in
             '("Twitter Color Emoji"
               "NotoEmoji Nerd Font Mono"
               "Noto Color Emoji"
               "Apple Color Emoji")
             when (font-installed-p font)
             return (if (>= emacs-major-version 28)
                        (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; FIXME: no working
    ;; Set mode-line font
    ;; (cl-loop for font in
    ;;          '("Menlo"
    ;;            "SF Pro Display"
    ;;            "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 90)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 90))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 90)))


    ;; FIXME: This cause wrong display of all-the-icons (in treemacs, dired, minibuffer, and etc.)
    ;; Specify font for all unicode characters
    ;; (cl-loop for font in '("Symbols Nerd Font" "Segoe UI Symbol" "Symbola" "Symbol")
    ;;          when (font-installed-p font)
    ;;          return (set-fontset-font t 'unicode font nil 'prepend))

    ))

(centaur-setup-fonts)

(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)
