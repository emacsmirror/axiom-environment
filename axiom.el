;;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; axiom.el -- a load-file for the axiom-environment system

;; Copyright (C) 2013 - 2015 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; This is an umbrella load-file that sets a few variables and pulls
;; in the axiom-environment system.
;;
;; Use it by putting
;;
;;   (load-file "/FULL/PATH/NAME/OF/THIS/FILE")
;;
;; into your .emacs file.
;;
;; Alternatively, if you want to be more selective about what global
;; variables (e.g. auto-mode-alist) get modified, then instead
;; copy-and-paste any wanted bits from this file into your .emacs
;; file, ensuring that this directory is also in your `load-path'.

;;; Code:

;; To ensure this directory is in the load-path
(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))

;; Uncomment this to put the ``themes'' subdirectory in the custom theme load-path
;;(when (and load-file-name (boundp 'custom-theme-load-path))
;;  (add-to-list 'custom-theme-load-path (concat (file-name-directory load-file-name) "themes/")))

;; Load the system
(require 'axiom-environment)

;; Setup auto-mode-alist
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.input" . axiom-input-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.spad" . axiom-spad-mode))

;; Uncomment this to enable axiom-selector on C-c a
;;(global-set-key (kbd "C-c a") 'axiom-selector)
