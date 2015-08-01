;;; axiom.el --- A load-file for the axiom-environment system -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2015 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; This is a simple load-file to set up the ``axiom-environment'' and
;; ``ob-axiom'' packages without using the Emacs packaging system.
;; Use it by putting
;;
;;   (load-file "/FULL/PATH/NAME/OF/THIS/FILE")
;;
;; into your .emacs file.

;;; Code:

;; To ensure this directory is in the load-path
(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))

;; Uncomment this to put the ``themes'' subdirectory in the custom theme load-path
;;(when (and load-file-name (boundp 'custom-theme-load-path))
;;  (add-to-list 'custom-theme-load-path (concat (file-name-directory load-file-name) "themes/")))

;; Load the axiom-environment system
(require 'axiom-environment)

;; Load the org-babel interface
(require 'ob-axiom)

;; Uncomment this to enable axiom-selector on C-c a
;;(global-set-key (kbd "C-c a") 'axiom-selector)

;;; axiom.el ends here
