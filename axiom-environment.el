;;; axiom-environment.el --- An environment for using Axiom/OpenAxiom/FriCAS -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2015 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; Package-Requires: ((emacs "24.2"))

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; To use this system, ensure this directory is in your `load-path',
;; and put
;;
;;   (require 'axiom-environment)
;;
;; into your .emacs file.

;;; Code:

;; Load everything
(require 'axiom-help-mode)
(require 'axiom-process-mode)
(require 'axiom-input-mode)
(require 'axiom-spad-mode)
(require 'axiom-buffer-menu)
(require 'axiom-selector)

;; Automatically put .input and .spad files into the correct major mode.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.input" . axiom-input-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.spad" . axiom-spad-mode))

;; Acknowledge we're loaded
(provide 'axiom-environment)

;;; axiom-environment.el ends here
