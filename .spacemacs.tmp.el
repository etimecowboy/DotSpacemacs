;;   ;; arrow keys in terminal
;;   ;;; xterm key decoding
;;   ;; As of this writing, emacs does not correctly recognize some xterm
;;   ;; key sequences.  Add code to deal with these.
;;   (defun add-escape-key-mapping-alist (escape-prefix key-prefix
;;                                                      suffix-alist)
;;     "Add mappings for up, down, left and right keys for a given list
;; of escape sequences and list of keys."
;;     (while suffix-alist
;;       (let ((escape-suffix (car (car suffix-alist)))
;;             (key-suffix (cdr (car suffix-alist))))
;;         (define-key input-decode-map (concat escape-prefix escape-suffix)
;;           (read-kbd-macro (concat key-prefix key-suffix))))
;;       (setq suffix-alist (cdr suffix-alist))))

;;   (defun my-setup-input-decode-map ()
;;     (setq nav-key-pair-alist
;;           '(("A" . "<up>") ("B" . "<down>")
;;             ("C" . "<right>") ("D" . "<left>")
;;             ("H" . "<home>") ("F" . "<end>")))

;;   (add-escape-key-mapping-alist "\e[1;2" "S-" nav-key-pair-alist)
;;   (add-escape-key-mapping-alist "\e[1;3" "M-" nav-key-pair-alist)
;;   (add-escape-key-mapping-alist "\e[1;4" "M-S-" nav-key-pair-alist)
;;   (add-escape-key-mapping-alist "\e[1;6" "C-S-" nav-key-pair-alist)
;;   (add-escape-key-mapping-alist "\e[1;7" "M-C-" nav-key-pair-alist)
;;   (add-escape-key-mapping-alist "\e[1;8" "M-C-S-" nav-key-pair-alist))

  ;; (unless (display-graphic-p)
  ;;   (progn
  ;;     (define-key input-decode-map "\e[1;2A" [(shift up)])
  ;;     (define-key input-decode-map "\e[1;2B" [(shift down)])
  ;;     (define-key input-decode-map "\e[1;2C" [(shift right)])
  ;;     (define-key input-decode-map "\e[1;2D" [(shift left)])
  ;;     (define-key input-decode-map "\e[1;3A" [(alt up)])
  ;;     (define-key input-decode-map "\e[1;3B" [(alt down)])
  ;;     (define-key input-decode-map "\e[1;3C" [(alt right)])
  ;;     (define-key input-decode-map "\e[1;3D" [(alt left)])
  ;;     (define-key input-decode-map "\e[1;5A" [(control up)])
  ;;     (define-key input-decode-map "\e[1;5B" [(control down)])
  ;;     (define-key input-decode-map "\e[1;5C" [(control right)])
  ;;     (define-key input-decode-map "\e[1;5D" [(control left)])
  ;;     (define-key input-decode-map "\e[1;7A" [(control alt up)])
  ;;     (define-key input-decode-map "\e[1;7B" [(control alt down)])
  ;;     (define-key input-decode-map "\e[1;7C" [(control alt right)])
  ;;     (define-key input-decode-map "\e[1;7D" [(control alt left)])
  ;;     ))

  ;; xterm with the resource ?.VT100.modifyOtherKeys: 1
  ;; GNU Emacs >=24.4 sets xterm in this mode and define
  ;; some of the escape sequences but not all of them.
;;   (defun character-apply-modifiers (c &rest modifiers)
;;     "Apply modifiers to the character C.
;; MODIFIERS must be a list of symbols amongst (meta control shift).
;; Return an event vector."
;;     (if (memq 'control modifiers) (setq c (if (or (and (<= ?@ c) (<= c ?_))
;;                                                   (and (<= ?a c) (<= c ?z)))
;;                                               (logand c ?\x1f)
;;                                             (logior (lsh 1 26) c))))
;;     (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
;;     (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
;;     (vector c))
;;   (defun my-eval-after-load-xterm ()
;;     (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
;;       (let ((c 32))
;;         (while (<= c 126)
;;           (mapc (lambda (x)
;;                   (define-key xterm-function-map (format (car x) c)
;;                     (apply 'character-apply-modifiers c (cdr x))))
;;                 '(;; with ?.VT100.formatOtherKeys: 0
;;                   ("\e\[27;3;%d~" meta)
;;                   ("\e\[27;5;%d~" control)
;;                   ("\e\[27;6;%d~" control shift)
;;                   ("\e\[27;7;%d~" control meta)
;;                   ("\e\[27;8;%d~" control meta shift)
;;                   ;; with ?.VT100.formatOtherKeys: 1
;;                   ("\e\[%d;3u" meta)
;;                   ("\e\[%d;5u" control)
;;                   ("\e\[%d;6u" control shift)
;;                   ("\e\[%d;7u" control meta)
;;                   ("\e\[%d;8u" control meta shift)))
;;           (setq c (1+ c))))))
;;   (eval-after-load "xterm" '(my-eval-after-load-xterm))
