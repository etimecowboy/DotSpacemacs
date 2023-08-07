;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- python layer packages file for Spacemacs.
;; Time-stamp: <2023-08-05 Sat 16:26 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst python-packages
  '(
    blacken
    flycheck
    importmagic
    live-py-mode
    org
    pip-requirements
    py-isort
    pydoc
    pytest
    jupyter
    sphinx-doc
    conda
    python
    anaconda-mode
    ;; eldoc
    )
  )

(defun python/init-blacken ()
  (use-package blacken
    :defer t
    :hook
    ((python-mode . blacken-mode)
     (python-ts-mode . blacken-mode))
    ;; :init
    ;; (spacemacs//bind-python-formatter-keys)
    ;; (when (and python-format-on-save
    ;;            (eq 'black python-formatter))
    ;;   (add-hook 'python-mode-hook 'blacken-mode))
    ;; (add-hook 'python-mode-hook 'blacken-mode)
    :config
    (spacemacs|hide-lighter blacken-mode)
    ))

(defun python/post-init-flycheck ()
  (spacemacs/enable-flycheck 'python-mode)
  (spacemacs/enable-flycheck 'python-ts-mode)
  )

(defun python/init-importmagic ()
  (use-package importmagic
    :defer t
    :hook
    ((python-mode . importmagic-mode)
     (python-ts-mode . importmagic-mode))
    :init
    (spacemacs|diminish importmagic-mode " ⓘ" " [i]")
    :config
    ;; override importmagic-mode
    (define-minor-mode importmagic-mode
      "A mode that lets you autoimport unresolved Python symbols."
      :init-value nil
      :lighter " import"
      :keymap (let ((keymap (make-sparse-keymap)))
                (define-key keymap (kbd "C-c C-l") 'importmagic-fix-imports)
                keymap)
      (when (not (derived-mode-p 'python-base-mode))
        (error "Importmagic only works with Python buffers"))
      (if importmagic-mode
          (progn
            (condition-case nil
                (progn
                  (setq importmagic-server
                        (epc:start-epc (importmagic--epc-python-interpreter)
                                       (importmagic--epc-args)))
                  (add-hook 'kill-buffer-hook 'importmagic--teardown-epc)
                  (add-hook 'before-revert-hook 'importmagic--teardown-epc)
                  (importmagic--auto-update-index))
              (error (progn
                       (message "Importmagic and/or epc not found. importmagic.el will not be working.")
                       (importmagic-mode -1) ;; This should take it to the stop server section.
                       ))))
        (importmagic--stop-server)))
    ))

(defun python/init-live-py-mode ()
  (use-package live-py-mode
    :defer t
    :commands live-py-mode
    ))

(defun python/init-pip-requirements ()
  (use-package pip-requirements
    :defer t))

(defun python/init-py-isort ()
  (use-package py-isort
    :defer t
    ;; :init
    ;; (add-hook 'before-save-hook 'spacemacs//python-sort-imports)
    ))

(defun python/init-pydoc ()
  (use-package pydoc
    :defer t
    ))

(defun python/init-pytest ()
  (use-package pytest
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-last-failed
               pytest-pdb-last-failed
               pytest-module
               pytest-pdb-module)
    :config
    (add-to-list 'pytest-project-root-files "setup.cfg")))

(defun python/init-jupyter ()
  (use-package jupyter
    :defer t
    ))

(defun python/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :pre-init
    (add-list-to-list 'org-babel-load-languages
                      '((python . t)
                        (jupyter . t)
                        ))
    (add-list-to-list 'org-src-lang-modes
                      '(("python" . python)
                        ("jupyter" . python)
                        ))
    ))

(defun python/init-sphinx-doc ()
  (use-package sphinx-doc
    :defer t
    :hook
    ((python-mode . sphinx-doc-mode)
     (python-ts-mode . sphinx-doc-mode))
    :config
    (setq sphinx-doc-include-types t)
    (spacemacs|hide-lighter sphinx-doc-mode)
    ))

(defun python/init-conda ()
  (use-package conda
    :defer t
    :commands (conda-env-list
               conda-env-activate
               conda-env-deactivate
               conda-env-autoactivate-mode
               conda-env-activate-for-buffer)
    :custom
    ((conda-anaconda-home "/opt/miniconda3/"))
    ))

(defun python/init-python ()
  (use-package python
    :defer t
    :init
    (setq python-ts-mode-hook python-mode-hook)
    ))

(defun python/init-anaconda-mode ()
  (use-package anaconda-mode
    :defer t
    :hook
    ((python-mode . anaconda-mode)
     (python-ts-mode . anaconda-mode))
    :init
    (setq anaconda-mode-installation-directory
          (concat spacemacs-cache-directory "anaconda-mode"))
    :config
    (spacemacs|hide-lighter anaconda-mode)
    ;; requires official python layer
    ;; (add-to-list 'spacemacs-jump-handlers-python-mode
    ;;              '(anaconda-mode-find-definitions :async t)
    ;;              )
    ))

;; (defun python/post-init-eldoc ()
;;   (add-hook 'python-mode-local-vars-hook #'spacemacs//python-setup-eldoc))
