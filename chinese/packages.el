;;; packages.el --- Chinese Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq chinese-packages
      '(
        ;; (chinese-wbim :toggle (eq chinese-default-input-method 'wubi))
        ;; (pyim :toggle (eq chinese-default-input-method 'pinyin))
        ;; pyim
        ;; pyim-basedict
        (fcitx :toggle chinese-enable-fcitx)
        find-by-pinyin-dired
        ace-pinyin
        pangu-spacing
        org
        ;; (youdao-dictionary :toggle chinese-enable-youdao-dict)
        dictionary
        ))

(defun chinese/init-fcitx ()
  (use-package fcitx
    :init
    (fcitx-evil-turn-on)))

;; (defun chinese/init-chinese-wbim ()
;;   "Initialize chinese-wubi"
;;   (use-package chinese-wbim
;;     :if (eq 'wubi chinese-default-input-method)
;;     :init
;;     (progn
;;       (autoload 'chinese-wbim-use-package "chinese-wubi"
;;         "Another emacs input method")
;;       ;; Tooptip is not good enough, so disable it here.
;;       (setq chinese-wbim-use-tooltip nil)
;;       (register-input-method
;;        "chinese-wubi" "euc-cn" 'chinese-wbim-use-package
;;        "五笔" "汉字五笔输入法" "wb.txt")
;;       (require 'chinese-wbim-extra)
;;       (global-set-key ";" 'chinese-wbim-insert-ascii)
;;       (setq default-input-method 'chinese-wubi))))

;; (defun chinese/init-youdao-dictionary ()
;;   (use-package youdao-dictionary
;;     :if chinese-enable-youdao-dict
;;     :defer
;;     :config
;;     (progn
;;       ;; Enable Cache
;;       (setq url-automatic-caching t
;;             ;; Set file path for saving search history
;;             youdao-dictionary-search-history-file
;;             (concat spacemacs-cache-directory ".youdao")
;;             ;; Enable Chinese word segmentation support
;;             youdao-dictionary-use-chinese-word-segmentation t))))

;; (defun chinese/init-pyim ()
;;   (use-package pyim
;;     :if (eq 'pinyin chinese-default-input-method)
;;     :init
;;     (progn
;;       (setq pyim-use-tooltip t
;;             pyim-directory (expand-file-name "pyim/" spacemacs-cache-directory)
;;             pyim-dicts-directory (expand-file-name "dicts/" pyim-directory)
;;             pyim-dcache-directory (expand-file-name "dcache/" pyim-directory)
;;             pyim-personal-file (expand-file-name "pyim-personal.txt" pyim-directory)
;;             default-input-method "pyim")
;;       (evilified-state-evilify pyim-dicts-manager-mode pyim-dicts-manager-mode-map))))

;; REF: https://github.com/tumashu/pyim/blob/master/pyim.el
;; TODO config pyim
;; (defun chinese/init-pyim ()
;;   (use-package pyim
;;     :ensure nil
;;     :demand t
;;     :config
;;     ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
;;     (use-package pyim-basedict
;;       :ensure nil
;;       :config (pyim-basedict-enable))

;;     (setq default-input-method "pyim")

;;     ;; 我使用全拼
;;     (setq pyim-default-scheme 'quanpin)

;;     ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;;     ;; 我自己使用的中英文动态切换规则是：
;;     ;; 1. 光标只有在注释里面时，才可以输入中文。
;;     ;; 2. 光标前是汉字字符时，才能输入中文。
;;     ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;;     (setq-default pyim-english-input-switch-functions
;;                   '(pyim-probe-dynamic-english
;;                     pyim-probe-isearch-mode
;;                     pyim-probe-program-mode
;;                     pyim-probe-org-structure-template))

;;     (setq-default pyim-punctuation-half-width-functions
;;                   '(pyim-probe-punctuation-line-beginning
;;                     pyim-probe-punctuation-after-punctuation))

;;     ;; 开启拼音搜索功能
;;     (pyim-isearch-mode 1)

;;     ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
;;     ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
;;     ;; 手动安装 posframe 包。
;;     (setq pyim-page-tooltip 'popup)

;;     ;; 选词框显示5个候选词
;;     (setq pyim-page-length 5)

;;     :bind
;;     (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
;;      ("C-;" . pyim-delete-word-from-personal-buffer))))


(defun chinese/init-find-by-pinyin-dired ()
  (use-package find-by-pinyin-dired
    :defer t))

(defun chinese/init-ace-pinyin ()
  (use-package ace-pinyin
    :defer t
    :init
    (progn
      (if chinese-enable-avy-pinyin
          (setq ace-pinyin-use-avy t))
      (ace-pinyin-global-mode t)
      (spacemacs|hide-lighter ace-pinyin-mode))))

(defun chinese/init-pangu-spacing ()
  (use-package pangu-spacing
    :defer t
    :init (progn (global-pangu-spacing-mode 1)
                 (spacemacs|hide-lighter pangu-spacing-mode)
                 ;; Always insert `real' space in org-mode.
                 (add-hook 'org-mode-hook
                           '(lambda ()
                              (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))))

(defun chinese/post-init-org ()
  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents))))

(defun chinese/init-dictionary ()
  (use-package dictionary
    :defer t
    :init
    (progn
      ;; (dictionary-tooltip-mode 1)
      ;; (global-dictionary-tooltip-mode 1)
      )))

