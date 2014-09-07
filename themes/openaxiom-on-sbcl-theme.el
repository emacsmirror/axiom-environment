;;; openaxiom-on-sbcl-theme.el -- a OpenAxiom/SBCL flavour for the Axiom environment

;; Copyright (C) 2013 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Setup Axiom environment customizations so they're suitable for
;; OpenAxiom running on SBCL.

;;; Code:

(deftheme openaxiom-on-sbcl
  "Setup the Axiom environment for OpenAxiom running on SBCL.")

(custom-theme-set-variables 'openaxiom-on-sbcl
  '(axiom-process-program "open-axiom --no-server"))

(custom-theme-set-variables 'openaxiom-on-sbcl
  '(axiom-process-prompt-regexp "^.*([[:digit:]]+) ->"))

(custom-theme-set-variables 'openaxiom-on-sbcl
  '(axiom-process-break-prompt-regexp "^0]"))

(custom-theme-set-variables 'openaxiom-on-sbcl
  '(axiom-process-preamble ")set messages highlighting off"))

(custom-theme-set-variables 'openaxiom-on-sbcl
  '(axiom-standard-package-info-file "openaxiom-standard-package-info.el"))

(custom-theme-set-variables 'openaxiom-on-sbcl
  '(axiom-standard-domain-info-file "openaxiom-standard-domain-info.el"))

(custom-theme-set-variables 'openaxiom-on-sbcl
  '(axiom-standard-category-info-file "openaxiom-standard-category-info.el"))

(custom-theme-set-variables 'openaxiom-on-sbcl
  '(axiom-standard-operation-info-file "openaxiom-standard-operation-info.el"))

(provide-theme 'openaxiom-on-sbcl)
