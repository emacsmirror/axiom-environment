;;; axiom-on-gcl-theme.el -- an Axiom/GCL flavour for the Axiom environment

;; Copyright (C) 2014 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Setup Axiom environment customizations so they're suitable for
;; Axiom running on GCL.

;;; Code:

(deftheme axiom-on-gcl
  "Setup the Axiom environment for Axiom running on GCL.")

(custom-theme-set-variables 'axiom-on-gcl
  '(axiom-process-program "env AXIOM=/usr/local/axiom/mnt/MACOSX PATH=/usr/local/axiom/mnt/MACOSX/bin:/usr/bin:/bin /usr/local/axiom/mnt/MACOSX/bin/axiom -noht"))

(custom-theme-set-variables 'axiom-on-gcl
  '(axiom-process-prompt-regexp "^.*([[:digit:]]+) ->"))

(custom-theme-set-variables 'axiom-on-gcl
  '(axiom-process-break-prompt-regexp "^.+>>"))

(custom-theme-set-variables 'axiom-on-gcl
  '(axiom-process-preamble ""))

(custom-theme-set-variables 'axiom-on-gcl
  '(axiom-standard-package-info-file "axiom-standard-package-info.el"))

(custom-theme-set-variables 'axiom-on-gcl
  '(axiom-standard-domain-info-file "axiom-standard-domain-info.el"))

(custom-theme-set-variables 'axiom-on-gcl
  '(axiom-standard-category-info-file "axiom-standard-category-info.el"))

(custom-theme-set-variables 'axiom-on-gcl
  '(axiom-standard-operation-info-file "axiom-standard-operation-info.el"))

(provide-theme 'axiom-on-gcl)
