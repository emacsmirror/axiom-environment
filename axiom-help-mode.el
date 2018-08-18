;;; axiom-help-mode.el --- Major mode for Axiom help descriptions -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2016 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A major mode to use in Axiom help text buffers.

;;; Code:

(require 'axiom-base)

(defvar axiom-help-package-face  'axiom-package-name)
(defvar axiom-help-domain-face   'axiom-domain-name)
(defvar axiom-help-category-face 'axiom-category-name)

(defvar axiom-help-mode-syntax-table
  (copy-syntax-table axiom-common-syntax-table)
  "The Axiom help mode syntax table.")

(defvar axiom-help-font-lock-keywords
  (list (cons axiom-standard-package-names-regexp          'axiom-help-package-face)
        (cons axiom-standard-package-abbreviations-regexp  'axiom-help-package-face)
        (cons axiom-standard-domain-names-regexp           'axiom-help-domain-face)
        (cons axiom-standard-domain-abbreviations-regexp   'axiom-help-domain-face)
        (cons axiom-standard-category-names-regexp         'axiom-help-category-face)
        (cons axiom-standard-category-abbreviations-regexp 'axiom-help-category-face)))

(defvar axiom-help-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map axiom-common-keymap)
    (define-key map (kbd "q") 'quit-window)
    map)
  "The Axiom Help mode local keymap.")

(defvar axiom-help-mode-hook nil
  "Hook for customizing Axiom Help mode.")

;;;###autoload
(define-derived-mode axiom-help-mode fundamental-mode "Axiom Help"
  "Major mode for Axiom Help buffers."
  :group 'axiom
  (setq font-lock-defaults (list axiom-help-font-lock-keywords))
  (setq axiom-menu-compile-buffer-enable nil)
  (setq axiom-menu-compile-file-enable nil)
  (setq axiom-menu-read-buffer-enable nil)
  (setq axiom-menu-read-file-enable nil)
  (setq axiom-menu-read-region-enable t)
  (setq axiom-menu-read-pile-enable nil))

(provide 'axiom-help-mode)

;;; axiom-help-mode.el ends here
