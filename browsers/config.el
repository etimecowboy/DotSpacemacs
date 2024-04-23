;; FIXME: it seems `chinese-enable-avy-pinyin' is require by some package.
;;
;; let: Symbol’s value as variable is void: chinese-enable-avy-pinyin
(setq chinese-enable-avy-pinyin t)

(defvar xy:default-browser 'firefox
  "The default web browser for emacs.

Avaliable choices are:

- 'firefox (default)
- 'chrome
- 'brave
- 'eww
- 'w3m")
