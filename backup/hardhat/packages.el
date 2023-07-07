;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- hardhat layer packages file for Spacemacs.
;; Time-stamp: <2023-03-16 Thu 08:24 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:
(setq hardhat-packages
      '(
        hardhat
        ))

(defun hardhat/init-hardhat ()
  (use-package hardhat
               :init
               (global-hardhat-mode 1)
               :config
               (setq hardhat-basename-protected-regexps
                     '("~\\'" "\\.lock\\'" "\\.ix\\'" "\\`test\\.out\\'" "-autoloads\\.el\\'" "\\`Desktop\\.ini\\'" "\\`META\\.yml\\'" "\\`MYMETA\\.yml\\'" "\\`TAGS\\'" "\\`Thumbs\\.db\\'" "\\`\\.dropbox\\'" "\\`\\.dropbox\\.cache\\'" "\\`\\.emacs\\.desktop\\'" "\\`\\.emacs\\.desktop\\.lock\\'" "\\.orig\\'" "\\.rej\\'" "\\.bak\\'")
                     hardhat-fullpath-protected-regexps
                     '("~/\\.emacs\\.d/elpa/" "~/\\.cpan/" "~/\\.cabal/" "~/perl5/perlbrew/" "~/\\.npm/" "~/\\.virtualenv/" "~/\\.virthualenv/" "~/\\.rvm/" "/[._]build/" "/\\.bzr/" "/\\.coverage/" "/\\.git/" "/\\.hg/" "/\\.rspec/" "/\\.sass-cache/" "/\\.svn/" "/_MTN/" "/_darcs/" "/CVS/" "/pm_to_blib/" "/RCS/" "/SCCS/" "/blib/" "/test_output/" "~/\\.emacs\\.d/\\.cask/" "~/\\.cask/" "~/\\.cargo/" "~/\\.conda/" "~/\\.docker/" "~/\\.local/" "~/\\.rustup/" "~/\\.ssh/" "~/bin/")
                     hardhat-mode-lighter "Ⓐ")))
