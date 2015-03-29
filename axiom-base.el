;;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; axiom-base.el -- basic setup for the Axiom environment

;; Copyright (C) 2013 - 2015 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Basic setup for the Axiom environment.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;
(defgroup axiom nil
  "An environment for working with the Axiom computer algebra system.")

(defcustom axiom-standard-package-info-file "fricas-standard-package-info.el"
  "File from which to `read' standard package information."
  :type 'string
  :group 'axiom)

(defcustom axiom-standard-domain-info-file "fricas-standard-domain-info.el"
  "File from which to `read' standard domain information."
  :type 'string
  :group 'axiom)

(defcustom axiom-standard-category-info-file "fricas-standard-category-info.el"
  "File from which to `read' standard category information."
  :type 'string
  :group 'axiom)

(defcustom axiom-standard-operation-info-file "fricas-standard-operation-info.el"
  "File from which to `read' standard operation information."
  :type 'string
  :group 'axiom)

(defface axiom-package-name '((t (:foreground "blue")))
  "Face used for displaying package names."
  :group 'axiom)

(defface axiom-domain-name '((t (:foreground "darkgreen")))
  "Face used for displaying domain names."
  :group 'axiom)

(defface axiom-category-name '((t (:foreground "brown")))
  "Face used for displaying category names."
  :group 'axiom)

(defface axiom-operation-name '((t (:foreground "black")))
  "Face used for displaying operation names."
  :group 'axiom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions for generating/loading pre-computed data
;;
(defvar axiom-environment-data-dir
  (and load-file-name (concat (file-name-directory load-file-name) "data/"))
  "Axiom environment data directory.")

(defun axiom-write-data-file (obj filename)
  "Write OBJ to FILENAME using function `pp', the pretty-printer.

The directory in which to write the file defaults to the value of
the variable `axiom-environment-data-dir'. This can be overridden
by specifying a different path in the FILENAME string (either
relative or absolute)."
  (let ((default-directory axiom-environment-data-dir))
    (with-temp-buffer
      (pp obj (current-buffer))
      (write-region (point-min) (point-max) filename))))

(defun axiom-read-data-file (filename)
  "Read a Lisp object from FILENAME using function `read'.

The directory in which FILENAME resides is assumed to be the
value of the variable `axiom-environment-data-dir'. This can be
overridden by specifying a different path in the FILENAME
string (either relative or absolute)."
  (let ((default-directory axiom-environment-data-dir))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (read (current-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load standard package/domain/category/operation names files
;;
(message "Loading standard Axiom package information")

(defvar axiom-standard-package-info
  (axiom-read-data-file axiom-standard-package-info-file)
  "A list of standard Axiom package (abbrev . name) pairs.")

(defvar axiom-standard-package-names
  (mapcar 'cdr axiom-standard-package-info)
  "A list of standard Axiom package names.")
  
(defvar axiom-standard-package-names-regexp
  (concat "\\<" (regexp-opt (mapcar 'regexp-quote axiom-standard-package-names)) "\\>")
  "Regular expression for Axiom standard package names.")

(defvar axiom-standard-package-abbreviations
  (remove nil (mapcar 'car axiom-standard-package-info))
  "A list of standard Axiom package abbreviations.")

(defvar axiom-standard-package-abbreviations-regexp
  (concat "\\<" (regexp-opt (mapcar 'regexp-quote axiom-standard-package-abbreviations)) "\\>")
  "Regular expression for Axiom standard package abbreviations.")

(defvar axiom-standard-package-names-and-abbreviations
  (append axiom-standard-package-names
          axiom-standard-package-abbreviations)
  "Standard Axiom package names and abbreviations.")

(message "Loading standard Axiom domain information")

(defvar axiom-standard-domain-info
  (axiom-read-data-file axiom-standard-domain-info-file)
  "A list of standard Axiom domain (abbrev . name) pairs.")

(defvar axiom-standard-domain-names
  (mapcar 'cdr axiom-standard-domain-info)
  "A list of standard Axiom domain names.")
  
(defvar axiom-standard-domain-names-regexp
  (concat "\\<" (regexp-opt (mapcar 'regexp-quote axiom-standard-domain-names)) "\\>")
  "Regular expression for Axiom standard domain names.")

(defvar axiom-standard-domain-abbreviations
  (remove nil (mapcar 'car axiom-standard-domain-info))
  "A list of standard Axiom domain abbreviations.")

(defvar axiom-standard-domain-abbreviations-regexp
  (concat "\\<" (regexp-opt (mapcar 'regexp-quote axiom-standard-domain-abbreviations)) "\\>")
  "Regular expression for Axiom standard domain abbreviations.")

(defvar axiom-standard-domain-names-and-abbreviations
  (append axiom-standard-domain-names
          axiom-standard-domain-abbreviations)
  "Standard Axiom domain names and abbreviations.")

(message "Loading standard Axiom category information")

(defvar axiom-standard-category-info
  (axiom-read-data-file axiom-standard-category-info-file)
  "A list of standard Axiom category (abbrev . name) pairs.")

(defvar axiom-standard-category-names
  (mapcar 'cdr axiom-standard-category-info)
  "A list of standard Axiom category names.")
  
(defvar axiom-standard-category-names-regexp
  (concat "\\<" (regexp-opt (mapcar 'regexp-quote axiom-standard-category-names)) "\\>")
  "Regular expression for Axiom standard category names.")

(defvar axiom-standard-category-abbreviations
  (remove nil (mapcar 'car axiom-standard-category-info))
  "A list of standard Axiom category abbreviations.")

(defvar axiom-standard-category-abbreviations-regexp
  (concat "\\<" (regexp-opt (mapcar 'regexp-quote axiom-standard-category-abbreviations)) "\\>")
  "Regular expression for Axiom standard category abbreviations.")

(defvar axiom-standard-category-names-and-abbreviations
  (append axiom-standard-category-names
          axiom-standard-category-abbreviations)
  "Standard Axiom category names and abbreviations.")

(message "Loading standard Axiom operation information")

(defvar axiom-standard-operation-info
  (axiom-read-data-file axiom-standard-operation-info-file)
  "A list of standard Axiom operation names.")

(defvar axiom-standard-operation-names
  axiom-standard-operation-info
  "A list of standard Axiom operation names.")

(defvar axiom-standard-operation-names-regexp
  (concat "\\<" (regexp-opt (mapcar 'regexp-quote axiom-standard-operation-names)) "\\>")
  "Regular expression for Axiom standard operation names.")

;; Lists combining package, domain & category names and/or abbreviations
(defvar axiom-standard-constructor-names
  (append axiom-standard-package-names
          axiom-standard-domain-names
          axiom-standard-category-names)
  "Standard Axiom constructor names.")

(defvar axiom-standard-constructor-abbreviations
  (append axiom-standard-package-abbreviations
          axiom-standard-domain-abbreviations
          axiom-standard-category-abbreviations)
  "Standard Axiom constructor abbreviations.")

(defvar axiom-standard-constructor-names-and-abbreviations
  (append axiom-standard-constructor-names
          axiom-standard-constructor-abbreviations)
  "Standard Axiom constructor names and abbreviations.")

;; Lists combining all constructor and operation names and abbreviations
(defvar axiom-standard-names
  (append axiom-standard-constructor-names
          axiom-standard-operation-names)
  "Standard Axiom names (package, domain, category & operation).")

(defvar axiom-standard-names-and-abbreviations
  (append axiom-standard-constructor-names-and-abbreviations
          axiom-standard-operation-names)
  "Standard Axiom names and abbreviationsa.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common syntax table
;;
(defvar axiom-common-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?_ "\\" table)
    (modify-syntax-entry ?+ ". 12" table)
    (modify-syntax-entry ?- ". 12" table)
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
  "The Axiom environment common syntax table.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common keymap (including the ``Axiom'' menu)
;;
(defvar axiom-menu-compile-file-enable nil)
(defvar axiom-menu-read-file-enable nil)
(defvar axiom-menu-eval-region-enable nil)
(defvar axiom-menu-read-region-enable nil)

(make-variable-buffer-local 'axiom-menu-compile-file-enable)
(make-variable-buffer-local 'axiom-menu-read-file-enable)
(make-variable-buffer-local 'axiom-menu-eval-region-enable)
(make-variable-buffer-local 'axiom-menu-read-region-enable)

(defvar axiom-common-keymap
  (let ((map (make-sparse-keymap "Axiom"))
        (menu-map (make-sparse-keymap "Axiom")))
    (set-keymap-parent map prog-mode-map)
    ;; Key assignments
    (define-key map (kbd "C-c C-d p") 'axiom-process-show-package)
    (define-key map (kbd "C-c C-d d") 'axiom-process-show-domain)
    (define-key map (kbd "C-c C-d c") 'axiom-process-show-category)
    (define-key map (kbd "C-c C-d k") 'axiom-process-show-constructor)
    (define-key map (kbd "C-c C-d o") 'axiom-process-display-operation)
    (define-key map (kbd "C-c C-d a") 'axiom-process-apropos-thing-at-point)
    (define-key map (kbd "C-c C-w") 'axiom-process-webview-constructor)
    (define-key map (kbd "C-c C-s") 'axiom-process-edit-constructor-source)
    (define-key map (kbd "C-c C-k") 'axiom-process-compile-file)
    (define-key map (kbd "C-c C-r") 'axiom-process-read-file)
    (define-key map (kbd "C-c C-e") 'axiom-process-eval-region)
    (define-key map (kbd "C-c C-y") 'axiom-process-read-region)
    ;; Menu items
    (define-key map [menu-bar axiom-menu] (cons "Axiom" menu-map))
    (define-key menu-map [axiom-menu-run-axiom]
      '(menu-item "Run Axiom" run-axiom))
    (define-key menu-map [axiom-menu-separator-3]
      '(menu-item "--"))
    (define-key menu-map [axiom-menu-read-file]
      '(menu-item "Read File..." axiom-process-read-file
                  :enable axiom-menu-read-file-enable))
    (define-key menu-map [axiom-menu-read-region]
      '(menu-item "Read Region..." axiom-process-read-region
                  :enable axiom-menu-read-region-enable))
    (define-key menu-map [axiom-menu-eval-region]
      '(menu-item "Eval Region" axiom-process-eval-region
                  :enable axiom-menu-eval-region-enable))
    (define-key menu-map [axiom-menu-separator-2]
      '(menu-item "--"))
    (define-key menu-map [axiom-menu-compile-file]
      '(menu-item "Compile File..." axiom-process-compile-file
                  :enable axiom-menu-compile-file-enable))
    (define-key menu-map [axiom-menu-separator-1]
      '(menu-item "--"))
    (define-key menu-map [axiom-menu-webview-constructor]
      '(menu-item "View Constructor Web Doc..." axiom-process-webview-constructor))
    (define-key menu-map [axiom-menu-edit-constructor-source]
      '(menu-item "Find Constructor Source..." axiom-process-edit-constructor-source))
    (define-key menu-map [axiom-menu-separator-0]
      '(menu-item "--"))
    (define-key menu-map [axiom-menu-apropos]
      '(menu-item "Apropos (at point)..." axiom-process-apropos-thing-at-point))
    (define-key menu-map [axiom-menu-display-operation]
      '(menu-item "Display Operation..." axiom-process-display-operation))
    (define-key menu-map [axiom-menu-show-constructor]
      '(menu-item "Show Constructor..." axiom-process-show-constructor))
    (define-key menu-map [axiom-menu-show-category]
      '(menu-item "Show Category..." axiom-process-show-category))
    (define-key menu-map [axiom-menu-show-domain]
      '(menu-item "Show Domain..." axiom-process-show-domain))
    (define-key menu-map [axiom-menu-show-package]
      '(menu-item "Show Package..." axiom-process-show-package))
    map)
  "The Axiom environment keymap.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;
(defun axiom-move-to-next-line ()
  "Move to beginning of next line.

Move beyond current line and all subsequent
continuation-lines (underscores escape new lines) to the beginning
of the next non-blank line."
  (let ((posn nil)
        (n 1)
        (done nil))
    (while (not done)
      (let ((p (line-end-position n)))
        (cond ((eql p posn)
               (setq done t))
              ((eql (char-before p) ?_)
               (setq posn p)
               (incf n))
              (t
               (setq posn p)
               (setq done t)))))
    (goto-char posn)
    (re-search-forward "^.+$")
    (beginning-of-line)))

(defun axiom-get-rest-of-line ()
  "Return the remainder of the current line.

Return a string containing the remainder of the current
line (from point), and the concatenation of all subsequent
continuation-lines (underscores escape new lines)."
  (let ((posns nil)
        (n 1)
        (done nil))
    (while (not done)
      (let ((p (line-end-position n)))
        (cond ((eql p (car posns))
               (setq done t))
              ((eql (char-before p) ?_)
               (push p posns)
               (incf n))
              (t
               (push p posns)
               (setq done t)))))
    (let ((line "")
          (beg (point)))
      (dolist (end (reverse posns))
        (let ((end-excl-underscore (if (eql (char-before end) ?_) (1- end) end)))
          (setq line (concat line (buffer-substring-no-properties beg end-excl-underscore))))
        (setq beg (1+ end)))
      line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Developer utils
;;
(defvar axiom-debug nil)

(defmacro axiom-debug-message (msg)
  (if axiom-debug
      `(message ,msg)
    nil))

(defun axiom-force-reload ()
  (interactive)
  (load "axiom-base")
  (load "axiom-help-mode")
  (load "axiom-process-mode")
  (load "axiom-input-mode")
  (load "axiom-spad-mode")
  (load "axiom-buffer-menu")
  (load "axiom-selector"))

(provide 'axiom-base)
