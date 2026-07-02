;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; layers.el --- lsp-bridge layers File for Spacemacs
;; Time-stamp: <2026-07-02 Thu 09:05:04 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;    lsp-bridge requires "markdown.el" package, which is loaded in markdown
;;    layer. Moreover, I run lsp-bridge in a conda environment. The "conda.el"
;;    package, which manages conda environments in Emacs, is loaded in my own
;;    python layer "../python/packages.el#(use-package conda", which overrides
;;    the spacemacs python layer.

;;; Code:

(configuration-layer/declare-layer-dependencies '(markdown python))
