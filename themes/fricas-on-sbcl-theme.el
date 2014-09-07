;;; fricas-on-sbcl-theme.el -- a FriCAS/SBCL flavour for the Axiom environment

;; Copyright (C) 2013 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Setup Axiom environment customizations so they're suitable for
;; FriCAS running on SBCL.

;;; Code:

(deftheme fricas-on-sbcl
  "Setup the Axiom environment for FriCAS running on SBCL.")

(custom-theme-set-variables 'fricas-on-sbcl
  '(axiom-process-program "fricas -nosman"))

(custom-theme-set-variables 'fricas-on-sbcl
  '(axiom-process-prompt-regexp "^.*([[:digit:]]+) ->"))

(custom-theme-set-variables 'fricas-on-sbcl
  '(axiom-process-break-prompt-regexp "^0]"))

(custom-theme-set-variables 'fricas-on-sbcl
  '(axiom-process-preamble ""))

(custom-theme-set-variables 'fricas-on-sbcl
  '(axiom-standard-package-info-file "fricas-standard-package-info.el"))

(custom-theme-set-variables 'fricas-on-sbcl
  '(axiom-standard-domain-info-file "fricas-standard-domain-info.el"))

(custom-theme-set-variables 'fricas-on-sbcl
  '(axiom-standard-category-info-file "fricas-standard-category-info.el"))

(custom-theme-set-variables 'fricas-on-sbcl
  '(axiom-standard-operation-info-file "fricas-standard-operation-info.el"))

(provide-theme 'fricas-on-sbcl)
