;;; company-axiom.el --- A company-mode backend for the axiom-environment system -*- lexical-binding: t -*-

;; Copyright (C) 2016 - 2019 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS, axiom-environment

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;; Package-Requires: ((emacs "24") (company "0.9") (axiom-environment "20171021"))

;;; Commentary:

;; --------------------------------------------------------------------
;; IMPORTANT: This package is DEPRECATED and no further development
;; will take place.  It has been replaced by the ``frimacs'' package
;; (http://github.com/pdo/frimacs).  To switch, first uninstall the
;; axiom-environment, company-axiom and ob-axiom packages, then
;; install frimacs and (optionally) ob-fricas.  These packages are
;; available in the MELPA package collection.
;; --------------------------------------------------------------------

;; Backend routines to support company-mode name completion in
;; axiom-environment buffers.

;;; Code:

(require 'cl-lib)

(require 'axiom-environment)

;;;###autoload
(defun company-axiom-backend (command &optional arg &rest ignored)
  "A company backend for axiom-environment.
See company documentation for COMMAND, ARG and IGNORED syntax."
  (interactive
   (company-begin-backend 'company-axiom-backend))
  (cl-case command
    (prefix
     (and (or (eql major-mode 'axiom-process-mode)
              (eql major-mode 'axiom-input-mode)
              (eql major-mode 'axiom-spad-mode))
          (company-grab-symbol)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (if (eql major-mode 'axiom-spad-mode)
          axiom-standard-names
        axiom-standard-names-and-abbreviations)))
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
         (cons (cl-first src-info) (cl-second src-info)))))))

;;;###autoload
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-axiom-backend))

(provide 'company-axiom)

;;; company-axiom.el ends here
