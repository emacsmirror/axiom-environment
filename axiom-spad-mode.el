;;; axiom-spad-mode.el --- Major mode for the Axiom library language -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2015 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A major mode for the SPAD language. SPAD is the library language used
;; by the Axiom computer algebra system.

;;; Code:

(require 'axiom-base)

(defface axiom-spad-keyword '((t (:foreground "grey25")))
  "Face used for displaying SPAD keywords."
  :group 'axiom)

(defvar axiom-spad-mode-syntax-table
  (copy-syntax-table axiom-common-syntax-table)
  "The Axiom SPAD mode syntax table.")

(defvar axiom-spad-keyword-names
  (list "add" "with" "has"
        "if" "then" "else"
        "for" "in" "by" "while" "repeat" "return" "break"))

(defvar axiom-spad-keywords-regexp
  (concat "\\<" (regexp-opt axiom-spad-keyword-names) "\\>")
  "Regular expression for SPAD keywords.")

(defvar axiom-spad-keyword-face  'axiom-spad-keyword)
(defvar axiom-spad-package-face  'axiom-package-name)
(defvar axiom-spad-domain-face   'axiom-domain-name)
(defvar axiom-spad-category-face 'axiom-category-name)

(defvar axiom-spad-font-lock-keywords
  (list (cons axiom-spad-keywords-regexp                   'axiom-spad-keyword-face)
        (cons axiom-standard-package-names-regexp          'axiom-spad-package-face)
        (cons axiom-standard-package-abbreviations-regexp  'axiom-spad-package-face)
        (cons axiom-standard-domain-names-regexp           'axiom-spad-domain-face)
        (cons axiom-standard-domain-abbreviations-regexp   'axiom-spad-domain-face)
        (cons axiom-standard-category-names-regexp         'axiom-spad-category-face)
        (cons axiom-standard-category-abbreviations-regexp 'axiom-spad-category-face)))

(defvar axiom-spad-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map axiom-common-keymap)
    map)
  "The Axiom SPAD mode local keymap.")

(defvar axiom-spad-mode-hook nil
  "Hook for customizing Axiom SPAD mode.")

;;;###autoload
(define-derived-mode axiom-spad-mode prog-mode "Axiom SPAD"
  "Major mode for Axiom's SPAD language."
  :group 'axiom
  (setq font-lock-defaults (list axiom-spad-font-lock-keywords))
  (setq axiom-menu-compile-buffer-enable t)
  (setq axiom-menu-compile-file-enable t)
  (setq axiom-menu-read-buffer-enable nil)
  (setq axiom-menu-read-file-enable nil)
  (setq axiom-menu-read-region-enable t)
  (setq axiom-menu-eval-region-enable t))

(provide 'axiom-spad-mode)

;;; axiom-spad-mode.el ends here
