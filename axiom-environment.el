;;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; axiom-environment.el -- an environment for using Axiom/OpenAxiom/FriCAS

;; Copyright (C) 2013 - 2014 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

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

;; Acknowledge we're loaded
(provide 'axiom-environment)
