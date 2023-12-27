;; NOTE: This solution is replaced by `path-headerline-mode'
;;
;; display file path in header line
;;
;; REF: https://www.emacswiki.org/emacs/HeaderLine

(defun with-face (str &rest face-plist)
    (propertize str 'face face-plist))

(defun sl/make-header ()
  ""
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]"))
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat (with-face sl/header
                             ;; :background "red"
                             :foreground "#8fb28f"
                             :weight 'bold
                             )))
      (concat (with-face sl/header
                         ;; :background "green"
                         ;; :foreground "black"
                         :weight 'bold
                         :foreground "#8fb28f"
                         )
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))


(defun xy/turn-on-header-line ()
  "Turn on header-line."
  (interactive)
  (setq header-line-format
        '("" ;; invocation-name
          (:eval (if (buffer-file-name)
                     (sl/make-header)
                   "%b")))))


(defun xy/turn-off-header-line ()
  "Turn off header-line."
  (interactive)
  (setq header-line-format nil))


(defun xy/toggle-header-line ()
  "Toggle header-line."
  (interactive)
  (if header-line-format
      (setq header-line-format nil)
    (setq header-line-format
          '("" ;; invocation-name
            (:eval (if (buffer-file-name)
                       (sl/make-header)
                     "%b"))))))

(add-hook 'buffer-list-update-hook #'xy/turn-on-header-line)
