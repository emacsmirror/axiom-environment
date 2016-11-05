;;; axiom-company.el --- support for company-mode -*- lexical-binding: t -*-

;; Copyright (C) 2016 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Backend routines to support company-mode name completion.

;;; Code:

(require 'cl-lib)
(require 'company)

(require 'axiom-base)
(require 'axiom-help-mode)
(require 'axiom-process-mode)

(defun axiom-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'axiom-company-backend))
    (prefix (and (or (eql major-mode 'axiom-process-mode)
                     (eql major-mode 'axiom-input-mode)
                     (eql major-mode 'axiom-spad-mode))
                 (company-grab-symbol)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      axiom-standard-names-and-abbreviations))
    (annotation
     (cl-case (car (axiom-process-constructor-type arg))
       (:package  " [P]")
       (:domain   " [D]")
       (:category " [C]")))
    (doc-buffer
     (cond ((not (get-buffer axiom-process-buffer-name))
            nil)
           ((axiom-process-verify-operation-name arg)
            (axiom-process-document-operation arg))
           ((axiom-process-verify-constructor-name-or-abbrev arg)
            (axiom-process-document-constructor arg))))
    (location
     (when (axiom-process-verify-constructor-name-or-abbrev arg)
       (let ((src-info (axiom-process-find-constructor-source arg)))
         (cons (first src-info) (second src-info)))))))

(add-to-list 'company-backends 'axiom-company-backend)

(provide 'axiom-company)

;;; axiom-company.el ends here
