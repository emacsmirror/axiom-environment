;;; axiom-build-utils.el --- Utilities to help build the Axiom environment -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2017 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Some useful functions to help build the Axiom environment.

;; This file is intentionally not loaded as part of the
;; axiom-environment package.  Instead it is suggested to use
;; `emacs-lisp-byte-compile-and-load' on this file to access these
;; functions.

;;; Code:

(require 'package-build)
(require 'package-x)

(require 'axiom-base)
(require 'axiom-process-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data file creation routines

(defvar axiom-build-query-buffer-name "*axiom-build-query*"
  "Name of buffer in which to process Axiom query results.")

(defun axiom-get-constructor-names-list (type)
  "Query the Axiom process and return a list of constructor names.

TYPE should be either :package, :domain or :category."
  (with-current-buffer (get-buffer-create axiom-build-query-buffer-name)
    (erase-buffer)
    (axiom-process-redirect-send-command
     (cond ((eql type :package)
            "((getDatabase$OperationsQuery) \"k\").('kind=\"p\").'name")
           ((eql type :domain)
            "((getDatabase$OperationsQuery) \"k\").('kind=\"d\").'name")
           ((eql type :category)
            "((getDatabase$OperationsQuery) \"k\").('kind=\"c\").'name"))
     (current-buffer))
    (goto-char (point-min))
    (let ((names nil))
      (while (re-search-forward "\"\\([^\"]+\\)\"" nil t)
        (setq names (cons (match-string 1) names)))
      (reverse names))))

(defun axiom-get-operation-names-list ()
  "Query the Axiom process and return a list of operation names."
  (with-current-buffer (get-buffer-create axiom-build-query-buffer-name)
    (erase-buffer)
    (axiom-process-redirect-send-command
     "((getDatabase$OperationsQuery) \"o\").'name"
     (current-buffer))
    (goto-char (point-min))
    (let ((names nil))
      (while (re-search-forward "\"\\([^\"]+\\)\"" nil t)
        (let ((name (match-string 1)))
          (unless (member name names)
            (setq names (cons name names)))))
      (reverse names))))

(defun axiom-get-abbreviation (constructor-name)
  "Return the abbreviation for the given constructor name."
  (with-current-buffer (get-buffer-create axiom-build-query-buffer-name)
    (erase-buffer)
    (axiom-process-redirect-send-command
     (format ")abbrev query %s" constructor-name)
     (current-buffer))
    (goto-char (point-min))
    (when (re-search-forward "\\([[:word:]]+\\)[[:space:]]+abbreviates[[:space:]]+\\(package\\|domain\\|category\\)[[:space:]]+\\([[:word:]]+\\)" nil t)
      (match-string 1))))

(defun axiom-make-abbreviations-alist (names-list)
  "Return a list of (abbrev . name) pairs."
  (mapcar (lambda (name)
            (cons (axiom-get-abbreviation name) name))
          names-list))

(defun axiom-make-standard-package-info-file ()
  (let ((default-directory axiom-environment-data-dir))
    (axiom-write-data-file (axiom-make-abbreviations-alist
                            (axiom-get-constructor-names-list :package))
                           axiom-standard-package-info-file)))

(defun axiom-make-standard-domain-info-file ()
  (let ((default-directory axiom-environment-data-dir))
    (axiom-write-data-file (axiom-make-abbreviations-alist
                            (axiom-get-constructor-names-list :domain))
                           axiom-standard-domain-info-file)))

(defun axiom-make-standard-category-info-file ()
  (let ((default-directory axiom-environment-data-dir))
    (axiom-write-data-file (axiom-make-abbreviations-alist
                            (axiom-get-constructor-names-list :category))
                           axiom-standard-category-info-file)))

(defun axiom-make-standard-operation-info-file ()
  (let ((default-directory axiom-environment-data-dir))
    (axiom-write-data-file (axiom-get-operation-names-list)
                           axiom-standard-operation-info-file)))

(defun axiom-make-standard-info-files ()
  (interactive)
  (axiom-make-standard-package-info-file)
  (axiom-make-standard-domain-info-file)
  (axiom-make-standard-category-info-file)
  (axiom-make-standard-operation-info-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs package creation routines

(defvar axiom-build-source-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(defun axiom-gen-version-string ()
  (format-time-string "%Y%m%d.%H%M" (current-time)))

(defvar axiom-environment-filespecs
  '("*.el" ("data" "data/*.el") ("themes" "themes/*.el")
    (:exclude "axiom.el" "ob-axiom.el")))

(defun axiom-environment-make-emacs-package (src-dir pkg-dir pkg-ver)
  "Build and upload the axiom-environment Emacs package."
  (interactive (list (read-directory-name "Project source directory: " axiom-build-source-dir)
                     (read-directory-name "Package archive directory: ")
                     (read-string "Version string: " (axiom-gen-version-string))))
  (unless (file-accessible-directory-p src-dir)
    (error "Cannot write to directory: %s" src-dir))
  (unless (file-accessible-directory-p pkg-dir)
    (error "Cannot write to directory: %s" pkg-dir))
  (package-build-package "axiom-environment" pkg-ver
                         axiom-environment-filespecs src-dir src-dir)
  (let ((package-archive-upload-base pkg-dir))
    (package-upload-file (concat src-dir "axiom-environment-" pkg-ver ".tar"))))

(defun ob-axiom-make-emacs-package (src-dir pkg-dir pkg-ver)
  "Build and upload the ob-axiom Emacs package."
  (interactive (list (read-directory-name "Project source directory: " axiom-build-source-dir)
                     (read-directory-name "Package archive directory: ")
                     (read-string "Version string: " (axiom-gen-version-string))))
  (unless (file-accessible-directory-p src-dir)
    (error "Cannot write to directory: %s" src-dir))
  (unless (file-accessible-directory-p pkg-dir)
    (error "Cannot write to directory: %s" pkg-dir))
  (package-build-package "ob-axiom" pkg-ver
                         '("ob-axiom.el") src-dir src-dir)
  (let ((package-archive-upload-base pkg-dir))
    (package-upload-file (concat src-dir "ob-axiom-" pkg-ver ".el"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Force reload of all source files from this directory

(defvar axiom-build-source-files
  '("axiom-base"
    "axiom-help-mode"
    "axiom-process-mode"
    "axiom-input-mode"
    "axiom-spad-mode"
    "axiom-boot-mode"
    "axiom-buffer-menu"
    "axiom-selector"
    "axiom-company"
    "ob-axiom"))

(defun axiom-force-compile ()
  (interactive)
  (dolist (file axiom-build-source-files)
    (byte-compile-file (concat axiom-build-source-dir file ".el"))))

(defun axiom-force-load ()
  (interactive)
  (dolist (file axiom-build-source-files)
    (load (concat axiom-build-source-dir file))))

(defun axiom-force-build ()
  (interactive)
  (axiom-force-compile)
  (axiom-force-load))

;;; axiom-build-utils.el ends here
