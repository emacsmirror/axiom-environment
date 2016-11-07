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

;;;###autoload
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

;;;###autoload
(add-to-list 'company-backends 'axiom-company-backend)

;; Augment standard company-mode key bindings
;;
;; The "C-h" and "C-w" key bindings in company-mode allow you to
;; temporarily display documentation and source-code, respectively,
;; when the completion menu is showing.  However, they do not allow
;; you to jump to these buffers.  So we add some extra bindings that
;; do this: "C-c C-d" and "C-c C-s", respectively.
(defun axiom-company-display-doc-buffer ()
  "Jump to the documentation buffer for the current selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (error "No documentation available"))))
    (with-current-buffer doc-buffer
      (goto-char (point-min)))
    (let* ((action '(display-buffer-use-some-window (inhibit-same-window . t)))
           (popup (display-buffer doc-buffer action)))
      (when (and popup axiom-select-popup-windows)
        (select-window popup)))))

(define-key company-active-map (kbd "C-c C-d") #'axiom-company-display-doc-buffer)

(defun axiom-company-display-source-buffer ()
  "Jump to the source buffer for the current selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (location (company-call-backend 'location selected))
         (line (or (cdr location) (error "No location available")))
         (src-buffer (find-file-noselect (car location))))
    (with-current-buffer src-buffer
      (goto-char (point-min))
      (forward-line (1- line)))
    (let* ((action '(display-buffer-use-some-window (inhibit-same-window . t)))
           (popup (display-buffer src-buffer action)))
      (when (and popup axiom-select-popup-windows)
        (select-window popup)))))

(define-key company-active-map (kbd "C-c C-s") #'axiom-company-display-source-buffer)

(provide 'axiom-company)

;;; axiom-company.el ends here
