;;; axiom-spad-mode.el --- Major mode for the Axiom library language -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2017 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A major mode for the SPAD language. SPAD is the library language used
;; by the Axiom computer algebra system.

;;; Code:

(require 'axiom-base)
(require 'axiom-help-mode)
(require 'axiom-process-mode)

(defface axiom-spad-doc-comment '((t :inherit font-lock-doc-face))
  "Face used for displaying SPAD documentation comments."
  :group 'axiom)

(defface axiom-spad-keyword '((t :inherit font-lock-keyword-face))
  "Face used for displaying SPAD keywords."
  :group 'axiom)

(defcustom axiom-spad-indentation-step 2
  "Indentation step to use in Axiom SPAD mode buffers."
  :type 'integer
  :group 'axiom)

(defvar axiom-spad-mode-syntax-table
  (copy-syntax-table axiom-common-syntax-table)
  "The Axiom SPAD mode syntax table.")

(defvar axiom-spad-doc-comment-regexp
  "\\+\\+.*$"
  "A SPAD documentation comment.")

(defvar axiom-spad-keyword-names
  (list "add" "with" "has" "is"
        "if" "then" "else"
        "for" "in" "by" "while" "repeat" "return" "break"))

(defvar axiom-spad-keywords-regexp
  (concat "\\<" (regexp-opt axiom-spad-keyword-names) "\\>")
  "Regular expression for SPAD keywords.")

(defvar axiom-spad-doc-comment-face 'axiom-spad-doc-comment)
(defvar axiom-spad-keyword-face     'axiom-spad-keyword)
(defvar axiom-spad-package-face     'axiom-package-name)
(defvar axiom-spad-domain-face      'axiom-domain-name)
(defvar axiom-spad-category-face    'axiom-category-name)
(defvar axiom-spad-operation-face   'axiom-operation-name)

(defvar axiom-spad-font-lock-keywords
  (list (cons axiom-spad-doc-comment-regexp        'axiom-spad-doc-comment-face)
        (cons axiom-spad-keywords-regexp           'axiom-spad-keyword-face)
        (cons axiom-standard-package-names-regexp  'axiom-spad-package-face)
        (cons axiom-standard-domain-names-regexp   'axiom-spad-domain-face)
        (cons axiom-standard-category-names-regexp 'axiom-spad-category-face)))

(defvar axiom-spad-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map axiom-common-keymap)
    map)
  "The Axiom SPAD mode local keymap.")

(defvar axiom-spad-mode-hook nil
  "Hook for customizing Axiom SPAD mode.")

(defun axiom-spad-complete-symbol ()
  (and (looking-back "[[:word:]]+" nil t)
       (list (match-beginning 0)
             (match-end 0)
             axiom-standard-names)))

(defun axiom-spad-interactive-complete ()
  (interactive)
  (if (and (boundp 'company-mode) company-mode)
      (company-complete)
    (complete-symbol nil)))

(defvar axiom-spad-indentation-increase-regexp
  "\\(^[[:blank:]]*if\\|else$\\|repeat$\\|==$\\|:$\\|with\\|add\\)"
  "Increase next line's indentation level if matched.")

(defun axiom-spad-indent-line ()
  (if (eql (char-syntax (char-before)) ?w)
      (axiom-spad-interactive-complete)
    (let ((computed-indent (+ (axiom-find-previous-indent)
                              (axiom-compute-indent-increment
                               axiom-spad-indentation-increase-regexp
                               axiom-spad-indentation-step))))
      (if (or (eql (current-column) 0)
              (axiom-in-indent-space))
          (axiom-set-current-indent computed-indent)
        (axiom-set-current-indent (axiom-find-previous-indent (current-column)))))))

(defun axiom-spad-syntax-propertize (start end)
  ;; Highlight operation names
  (remove-text-properties start end '(font-lock-face nil))
  (goto-char start)
  (while (and (< (point) end) (re-search-forward "\\([[:word:]]+\\)\\([[:blank:]]+[[:word:]]\\|[[:blank:]]*(\\)" end t))
    (let ((matched (match-string 1)))
      (when (and matched (> (length matched) 1)
                 (member matched axiom-standard-operation-names))
        (put-text-property (match-beginning 1) (match-end 1)
                           'font-lock-face axiom-spad-operation-face)))
    (goto-char (1- (point))))  ; unswallow word or '(' character
  ;; Mark comment syntax
  (goto-char start)
  (funcall (syntax-propertize-rules
            ("\\(-\\)\\(-\\)"
             (1 (string-to-syntax "< 1"))
             (2 (string-to-syntax "< 2")))
            ("\\(\\+\\)\\(\\+\\)"
             (1 (string-to-syntax "< 1"))
             (2 (string-to-syntax "< 2"))))
           start end))

;;;###autoload
(define-derived-mode axiom-spad-mode prog-mode "Axiom SPAD"
  "Major mode for Axiom's SPAD language."
  :group 'axiom
  (setq font-lock-defaults (list axiom-spad-font-lock-keywords))
  (setq electric-indent-inhibit t)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'completion-at-point-functions)
  (make-local-variable 'syntax-propertize-function)
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-function)
  (setq indent-line-function 'axiom-spad-indent-line)
  (setq completion-at-point-functions '(axiom-spad-complete-symbol))
  (setq syntax-propertize-function (function axiom-spad-syntax-propertize))
  (setq adaptive-fill-first-line-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq adaptive-fill-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq fill-paragraph-function (function axiom-fill-paragraph))
  (setq axiom-menu-compile-buffer-enable t)
  (setq axiom-menu-compile-file-enable t)
  (setq axiom-menu-read-buffer-enable nil)
  (setq axiom-menu-read-file-enable nil)
  (setq axiom-menu-read-region-enable t)
  (setq axiom-menu-read-pile-enable nil))

(provide 'axiom-spad-mode)

;;; axiom-spad-mode.el ends here
