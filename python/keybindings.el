(spacemacs/declare-prefix-for-mode 'python-mode "mc" "conda")
(spacemacs/declare-prefix-for-mode 'python-mode "mh" "pydoc")
(spacemacs/declare-prefix-for-mode 'python-mode "mr" "refactoring")
(spacemacs/declare-prefix-for-mode 'python-mode "ms" "sphinx-doc")
(spacemacs/declare-prefix-for-mode 'python-mode "mt" "test")
(spacemacs/declare-prefix-for-mode 'python-mode "mg" "anaconda")

(spacemacs/declare-prefix-for-mode 'python-ts-mode "mc" "conda")
(spacemacs/declare-prefix-for-mode 'python-ts-mode "mh" "pydoc")
(spacemacs/declare-prefix-for-mode 'python-ts-mode "mr" "refactoring")
(spacemacs/declare-prefix-for-mode 'python-ts-mode "ms" "sphinx-doc")
(spacemacs/declare-prefix-for-mode 'python-ts-mode "mt" "test")
(spacemacs/declare-prefix-for-mode 'python-ts-mode "mg" "anaconda")

(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "l"  'live-py-mode
  "cl" 'conda-env-list
  "ca" 'conda-env-activate
  "cd" 'conda-env-deactivate
  "cA" 'conda-env-autoactivate-mode
  "cb" 'conda-env-activate-for-buffer
  "hp" 'pydoc-at-point-no-jedi
  "hP" 'pydoc
  "rb" 'blacken-buffer
  "ri" 'py-isort-buffer
  "rf" 'importmagic-fix-symbol-at-point
  "se" 'sphinx-doc-mode
  "sd" 'sphinx-doc
  "tA" 'pytest-pdb-all
  "ta" 'pytest-all
  "tB" 'pytest-pdb-module
  "tb" 'pytest-module
  "tl" 'pytest-again
  "tf" 'pytest-last-failed
  "tF" 'pytest-pdb-last-failed
  "tT" 'pytest-pdb-one
  "tt" 'pytest-one
  "tM" 'pytest-pdb-module
  "tm" 'pytest-module
  "hh" 'anaconda-mode-show-doc
  "ga" 'anaconda-mode-find-assignments
  "gb" 'xref-go-back
  "gu" 'anaconda-mode-find-references
  )

(spacemacs/set-leader-keys-for-major-mode 'python-ts-mode
  "l"  'live-py-mode
  "cl" 'conda-env-list
  "ca" 'conda-env-activate
  "cd" 'conda-env-deactivate
  "cA" 'conda-env-autoactivate-mode
  "cb" 'conda-env-activate-for-buffer
  "hp" 'pydoc-at-point-no-jedi
  "hP" 'pydoc
  "rb" 'blacken-buffer
  "ri" 'py-isort-buffer
  "rf" 'importmagic-fix-symbol-at-point
  "se" 'sphinx-doc-mode
  "sd" 'sphinx-doc
  "tA" 'pytest-pdb-all
  "ta" 'pytest-all
  "tB" 'pytest-pdb-module
  "tb" 'pytest-module
  "tl" 'pytest-again
  "tf" 'pytest-last-failed
  "tF" 'pytest-pdb-last-failed
  "tT" 'pytest-pdb-one
  "tt" 'pytest-one
  "tM" 'pytest-pdb-module
  "tm" 'pytest-module
  "hh" 'anaconda-mode-show-doc
  "ga" 'anaconda-mode-find-assignments
  "gb" 'xref-pop-marker-stack
  "gu" 'anaconda-mode-find-references
  )
