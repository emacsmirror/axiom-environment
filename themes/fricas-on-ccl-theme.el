;;; fricas-on-ccl-theme.el -- a FriCAS/CCL flavour for the Axiom environment

;; Copyright (C) 2016 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Setup Axiom environment customizations so they're suitable for
;; FriCAS running on CCL (Clozure Common Lisp).

;;; Code:

(deftheme fricas-on-ccl
  "Setup the Axiom environment for FriCAS running on CCL.")

(custom-theme-set-variables 'fricas-on-ccl
  '(axiom-process-program "fricas -nosman"))

(custom-theme-set-variables 'fricas-on-ccl
  '(axiom-process-prompt-regexp "^.*([[:digit:]]+) ->"))

(custom-theme-set-variables 'fricas-on-ccl
  '(axiom-process-break-prompt-regexp "^1 >"))

(custom-theme-set-variables 'fricas-on-ccl
  '(axiom-process-preamble ""))

(custom-theme-set-variables 'fricas-on-ccl
  '(axiom-standard-package-info-file "fricas-standard-package-info.el"))

(custom-theme-set-variables 'fricas-on-ccl
  '(axiom-standard-domain-info-file "fricas-standard-domain-info.el"))

(custom-theme-set-variables 'fricas-on-ccl
  '(axiom-standard-category-info-file "fricas-standard-category-info.el"))

(custom-theme-set-variables 'fricas-on-ccl
  '(axiom-standard-operation-info-file "fricas-standard-operation-info.el"))

(provide-theme 'fricas-on-ccl)
