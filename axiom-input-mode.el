;;; axiom-input-mode.el --- Major mode for the Axiom interactive language -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2017 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A major mode for the Axiom interactive language, e.g. .input files.

;;; Code:

(require 'axiom-base)
(require 'axiom-help-mode)
(require 'axiom-process-mode)
(require 'imenu)

(defface axiom-input-doc-comment '((t :inherit font-lock-doc-face))
  "Face used for displaying input documentation comments."
  :group 'axiom)

(defface axiom-input-keyword '((t :inherit font-lock-keyword-face))
  "Face used for displaying input file keywords."
  :group 'axiom)

(defcustom axiom-input-indentation-step 2
  "Indentation step to use in Axiom input mode buffers."
  :type 'integer
  :group 'axiom)

(defvar axiom-input-mode-syntax-table
  (copy-syntax-table axiom-common-syntax-table)
  "The Axiom input mode syntax table.")

(defvar axiom-input-doc-comment-regexp
  "\\+\\+.*$"
  "An Axiom documentation comment.")

(defvar axiom-input-keyword-names
  (list "has"
        "if" "then" "else"
        "for" "in" "by" "while" "repeat" "return" "break"))

(defvar axiom-input-keywords-regexp
  (concat "\\<" (regexp-opt axiom-input-keyword-names) "\\>")
  "Regular expression for input file keywords.")

(defvar axiom-input-imenu-generic-expression
  '(("Variable" "^\\([[:word:]]+\\).+:=" 1)
    ("Function" "^\\([[:word:]]+\\).+==\\([^>]\\|$\\)" 1)
    ("Macro" "^\\([[:word:]]+\\).+==>" 1))
  "Setting for `imenu-generic-expression' in Axiom input mode.")

(defvar axiom-input-doc-comment-face 'axiom-input-doc-comment)
(defvar axiom-input-keyword-face     'axiom-input-keyword)
(defvar axiom-input-package-face     'axiom-package-name)
(defvar axiom-input-domain-face      'axiom-domain-name)
(defvar axiom-input-category-face    'axiom-category-name)
(defvar axiom-input-operation-face   'axiom-operation-name)

(defvar axiom-input-font-lock-keywords
  (list (cons axiom-input-doc-comment-regexp               'axiom-input-doc-comment-face)
        (cons axiom-input-keywords-regexp                  'axiom-input-keyword-face)
        (cons axiom-standard-package-names-regexp          'axiom-input-package-face)
        (cons axiom-standard-package-abbreviations-regexp  'axiom-input-package-face)
        (cons axiom-standard-domain-names-regexp           'axiom-input-domain-face)
        (cons axiom-standard-domain-abbreviations-regexp   'axiom-input-domain-face)
        (cons axiom-standard-category-names-regexp         'axiom-input-category-face)
        (cons axiom-standard-category-abbreviations-regexp 'axiom-input-category-face)))

(defvar axiom-input-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map axiom-common-keymap)
    (define-key map (kbd "C-<return>") 'axiom-input-send-line)
    map)
  "The Axiom input mode local keymap.")

(defvar axiom-input-mode-hook nil
  "Hook for customizing Axiom input mode.")

(defun axiom-input-read-buffer ()
  (interactive)
  (axiom-process-read-file buffer-file-name))

(defun axiom-input-send-line ()
  (interactive)
  (let ((str (save-excursion
               (beginning-of-line)
               (axiom-get-rest-of-line))))
    (axiom-process-eval-string str)
    (axiom-move-to-next-line)))

(defun axiom-input-complete-symbol ()
  (and (looking-back "[[:word:]]+" nil t)
       (list (match-beginning 0)
             (match-end 0)
             axiom-standard-names-and-abbreviations)))

(defun axiom-input-interactive-complete ()
  (interactive)
  (if (and (boundp 'company-mode) company-mode)
      (company-complete)
    (complete-symbol nil)))

(defvar axiom-input-indentation-increase-regexp
  "\\(^[[:blank:]]*if\\|else$\\|repeat$\\|==$\\)"
  "When to increase next line's indentation level.")

(defun axiom-input-indent-line ()
  (if (eql (char-syntax (char-before)) ?w)
      (axiom-input-interactive-complete)
    (let ((computed-indent (+ (axiom-find-previous-indent)
                              (axiom-compute-indent-increment
                               axiom-input-indentation-increase-regexp
                               axiom-input-indentation-step))))
      (if (or (eql (current-column) 0)
              (axiom-in-indent-space))
          (axiom-set-current-indent computed-indent)
        (axiom-set-current-indent (axiom-find-previous-indent (current-column)))))))

(defun axiom-input-syntax-propertize (start end)
  ;; Highlight operation names
  (remove-text-properties start end '(font-lock-face nil))
  (goto-char start)
  (while (and (< (point) end) (re-search-forward "\\([[:word:]]+\\)\\([[:blank:]]+[[:word:]]\\|[[:blank:]]*(\\)" end t))
    (let ((matched (match-string 1)))
      (when (and matched (> (length matched) 1)
                 (member matched axiom-standard-operation-names))
        (put-text-property (match-beginning 1) (match-end 1)
                           'font-lock-face axiom-input-operation-face)))
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
(define-derived-mode axiom-input-mode prog-mode "Axiom Input"
  "Major mode for the Axiom-Input interactive language."
  :group 'axiom
  (setq font-lock-defaults (list 'axiom-input-font-lock-keywords))
  (setq electric-indent-inhibit t)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'completion-at-point-functions)
  (make-local-variable 'syntax-propertize-function)
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-function)
  (setq indent-line-function 'axiom-input-indent-line)
  (setq completion-at-point-functions '(axiom-input-complete-symbol))
  (setq syntax-propertize-function (function axiom-input-syntax-propertize))
  (setq adaptive-fill-first-line-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq adaptive-fill-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq fill-paragraph-function (function axiom-fill-paragraph))
  (setq imenu-generic-expression axiom-input-imenu-generic-expression)
  (setq axiom-menu-compile-buffer-enable nil)
  (setq axiom-menu-compile-file-enable nil)
  (setq axiom-menu-read-buffer-enable t)
  (setq axiom-menu-read-file-enable t)
  (setq axiom-menu-read-region-enable t)
  (setq axiom-menu-read-pile-enable t))

(provide 'axiom-input-mode)

;;; axiom-input-mode.el ends here
