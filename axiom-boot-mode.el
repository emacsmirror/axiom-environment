;;; axiom-boot-mode.el --- Major mode for the OpenAxiom/FriCAS Boot language -*- lexical-binding: t -*-

;; Copyright (C) 2015 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A major mode for the OpenAxiom & FriCAS Boot languages.  Boot is an
;; internal language of these computer algebra systems that is used to
;; implement the SPAD compiler and Input interpreter.  It has a
;; relatively direct mapping to Common Lisp.

;;; Code:

(require 'axiom-base)

(defface axiom-boot-keyword '((t :inherit font-lock-keyword-face))
  "Face used for displaying Boot keywords."
  :group 'axiom)

(defvar axiom-boot-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?_ "\\" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "w" table)
    (modify-syntax-entry ?! "w" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table)
  "The Axiom Boot mode syntax table.")

(defvar axiom-boot-keyword-names
  (list "and" "by" "case" "cross" "else" "for" "if" "in" "is" "isnt" "of"
        "or" "repeat" "return" "structure" "then" "until" "where" "while"))

(defvar axiom-boot-keywords-regexp
  (concat "\\<" (regexp-opt axiom-boot-keyword-names) "\\>")
  "Regular expression for Boot keywords.")

(defvar axiom-boot-keyword-face  'axiom-boot-keyword)

(defvar axiom-boot-font-lock-keywords
  (list (cons axiom-boot-keywords-regexp 'axiom-boot-keyword-face)))

(defvar axiom-boot-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map axiom-common-keymap)
    map)
  "The Axiom Boot mode local keymap.")

(defvar axiom-boot-mode-hook nil
  "Hook for customizing Axiom Boot mode.")

(defvar axiom-boot-syntax-propertize-fn
  (syntax-propertize-rules
   ("\\(-\\)\\(-\\)"     (1 (string-to-syntax "< 1"))
                         (2 (string-to-syntax "< 2")))
   ("\\(\\+\\)\\(\\+\\)" (1 (string-to-syntax "< 1"))
                         (2 (string-to-syntax "< 2")))))

;;;###autoload
(define-derived-mode axiom-boot-mode prog-mode "Axiom Boot"
  "Major mode for Axiom's internal Boot language."
  :group 'axiom
  (setq font-lock-defaults (list axiom-boot-font-lock-keywords))
  (make-local-variable 'syntax-propertize-function)
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-function)
  (setq syntax-propertize-function axiom-boot-syntax-propertize-fn)
  (setq adaptive-fill-first-line-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq adaptive-fill-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq fill-paragraph-function (function axiom-fill-paragraph))
  (setq axiom-menu-compile-buffer-enable nil)
  (setq axiom-menu-compile-file-enable nil)
  (setq axiom-menu-read-buffer-enable nil)
  (setq axiom-menu-read-file-enable nil)
  (setq axiom-menu-read-region-enable nil)
  (setq axiom-menu-read-pile-enable nil))

(provide 'axiom-boot-mode)

;;; axiom-boot-mode.el ends here
