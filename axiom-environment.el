;;; axiom-environment.el --- An environment for using Axiom/OpenAxiom/FriCAS -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2015 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; Package-Requires: ((emacs "24.2"))

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; The ``axiom-environment'' package is intended to make it easier to
;; work with, and understand, the Axiom, OpenAxiom and FriCAS computer
;; algebra systems.  It implements four different major modes for the
;; Emacs text editor:

;;   1. axiom-process-mode: for interaction with a running Axiom
;;   process.

;;   2. axiom-help-mode: for displaying help information about the
;;      Axiom system.
  
;;   3. axiom-input-mode: for editing a .input (Axiom script) file.

;;   4. axiom-spad-mode: for editing Axiom library code written in the
;;      SPAD language.

;; The main features of these modes (so far) are syntax highlighting
;; to display package, domain & category names (and their
;; abbreviations) in distinct colours, and to give quick access to
;; popup buffers displaying summary information about these types and
;; their operations.  The syntax highlighting feature allows us to see
;; at a glance which aspect of the type system we are concerned with
;; (domains or categories), and the popup buffer feature allows us to
;; examine (and re-examine) these types without interupting our
;; workflow (i.e. interaction in the Axiom REPL).  Hint: to make a
;; popup help buffer go away, press ``q'', emacs's window
;; configuration will be restored to what it was before the help
;; window appeared and the help buffer will be buried in the buffer
;; list.

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
