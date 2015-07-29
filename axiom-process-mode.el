;;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; axiom-process-mode.el -- a Comint-derived mode for Axiom

;; Copyright (C) 2013 - 2015 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A mode for interacting with a running Axiom process.

;;; Code:

(require 'axiom-base)
(require 'comint)

(defcustom axiom-process-buffer-name "*Axiom REPL*"
  "Default Axiom process buffer name.

Must begin and end with an asterisk."
  :type 'string
  :group 'axiom)

(defcustom axiom-process-program "fricas -nosman"
  "Command line to invoke the Axiom process."
  :type 'string
  :group 'axiom)

(defcustom axiom-process-prompt-regexp "^.*([[:digit:]]+) ->"
  "Regexp to recognize prompts from the Axiom process."
  :type 'regexp
  :group 'axiom)

(defcustom axiom-process-break-prompt-regexp "^0]"
  "Regexp to recognize a Lisp BREAK prompt."
  :type 'regexp
  :group 'axiom)

(defcustom axiom-process-preamble ""
  "Initial commands to push to the Axiom process."
  :type 'string
  :group 'axiom)

(defcustom axiom-process-compile-file-result-directory ""
  "Directory in which to place compilation results.

Only used when variable
`axiom-process-compile-file-use-result-directory' is non-NIL."
  :type 'string
  :group 'axiom)

(defcustom axiom-process-compile-file-use-result-directory nil
  "Non-nil to place compilation results in a central directory.

When non-nil place results in the directory named by variable
`axiom-process-compile-file-result-directory', otherwise they will be
placed in the same directory as the source file."
  :type 'boolean
  :group 'axiom)

(defcustom axiom-process-compile-file-buffer-name "*Axiom Compilation*"
  "A buffer in which to echo compiler output."
  :type 'string
  :group 'axiom)

(defcustom axiom-process-query-buffer-name "*Axiom Query*"
  "Axiom process query result buffer name."
  :type 'string
  :group 'axiom)

(defcustom axiom-process-webview-url "http://fricas.github.io/api/"
  "The base URL for SPAD constructor documentation."
  :type 'string
  :group 'axiom)

(defcustom axiom-process-spad-source-dirs
  '("./" "/usr/local/fricas/lib/fricas/target/i686-apple-darwin14.1.0/src/algebra/")
  "A list of directories in which to search for SPAD source code."
  :type 'list
  :group 'axiom)

(defvar axiom-process-mode-hook nil
  "Hook for customizing `axiom-process-mode'.")

(defvar axiom-process-mode-map
  (let ((map (copy-keymap axiom-common-keymap)))
    (set-keymap-parent map comint-mode-map)
    map)
  "Keymap for `axiom-process-mode'.")

(defvar axiom-process-not-running-message
  "Axiom process not running, try M-x run-axiom")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility macros
;;
(defmacro with-axiom-process-query-buffer (&rest body)
  "Set current-buffer to a query result buffer, with dynamic extent.

Use this instead of `with-temp-buffer' so that the buffer can be
easily examined when things go wrong.  The buffer switched to is
actually the buffer called `axiom-process-query-buffer-name', which is
cleared when the dynamic extent of this form is entered.

IMPORTANT NOTE: Unlike `with-temp-buffer', this means that nested
calls are NOT ALLOWED."
  `(with-current-buffer (get-buffer-create axiom-process-query-buffer-name)
     (fundamental-mode)
     (erase-buffer)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command utility functions
;;
(defun axiom-process-insert-command (command)
  "Send COMMAND, a string, to the running Axiom process.

The COMMAND and its output are inserted in the Axiom process buffer at
the current process-mark, which may be before the end of the buffer if
the user is part-way through editing the next command."
  (with-current-buffer axiom-process-buffer-name
    (let ((proc (get-buffer-process (current-buffer)))
          (command-text command)
          (pending-text ""))
      ;; Remove newlines from end of command string
      (while (and (> (length command-text) 0)
                  (char-equal ?\n (aref command-text (1- (length command-text)))))
        (setq command-text (substring command-text 0 (1- (length command-text)))))
      ;; Contrary to what it says in the documentation of `comint-send-input',
      ;; calling it sends _all_ text from the process mark to the _end_ of
      ;; the buffer to the process.  So we need to temporarily remove any
      ;; text the user is currently typing at the end of the buffer before
      ;; calling `comint-send-input', then restore it afterwards.
      (when (> (point-max) (process-mark proc))
        (setq pending-text (delete-and-extract-region (process-mark proc) (point-max))))
      (goto-char (process-mark proc))
      (insert command-text)
      (comint-send-input nil t)
      (insert pending-text))))

(defun axiom-process-redirect-send-command (command output-buffer &optional display echo-cmd echo-result output-command)
  "Send COMMAND to Axiom and put result in OUTPUT-BUFFER.

If DISPLAY is non-nil then display the result buffer.

If ECHO-CMD is non-nil then copy the command to the process buffer,
and if ECHO-RESULT is non-nil then also copy the result too.

If OUTPUT-COMMAND is non-nil then include command in output to
OUTPUT-BUFFER."
  (with-current-buffer axiom-process-buffer-name
    (save-excursion
      (let ((proc (get-buffer-process (current-buffer))))
        (when output-command
          (with-current-buffer output-buffer
            (insert command "\n")))
        (when echo-cmd
          (goto-char (process-mark proc))
          (insert-before-markers command))
        (comint-redirect-send-command command output-buffer echo-result (not display))
        (while (not comint-redirect-completed)
          (accept-process-output proc))
        (when (and echo-cmd (not echo-result))  ; get prompt back
          (axiom-process-insert-command ""))))))

(defun axiom-process-get-old-input ()
  "An Axiom-specific replacement for `comint-get-old-input'.

Return the concatenation of the current line and all subsequent
continuation-lines (underscores escape new lines)."
  (comint-bol)
  (axiom-get-rest-of-line))

(defun axiom-process-find-constructor-source (name-or-abbrev)
  "Attempt to find the SPAD source for the given constructor.

Invoke a grep shell-command looking in the directories specified by
`axiom-process-spad-source-dirs'.  Return a list containing
a filename and a line number."
  (let ((filename "")
	(line-number 0))
    (dolist (dir axiom-process-spad-source-dirs)
      (unless (> line-number 0)
	(let ((grep-out (with-temp-buffer
			  (shell-command
			   (concat "grep -n ')abbrev .*\\<" name-or-abbrev "\\>' " dir "*.spad")
			   t nil)
			  (buffer-substring-no-properties (point-min) (point-max)))))
	  (when (> (length grep-out) 0)
	    (string-match "\\(.+\\):\\(.+\\):" grep-out)
	    (setq filename (substring grep-out 0 (match-end 1)))
	    (setq line-number (string-to-number (substring grep-out (1+ (match-end 1)) (match-end 2))))))))
    (when (and (> (length filename) 0) (> line-number 0))
      (list filename line-number))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory tracking -- track Axiom's notion of ``current directory''
;;
(defun axiom-process-force-cd-update (&optional no-msg)
  "Force update of buffer-local variable `default-directory'.

Also return the directory as a string.  If NO-MSG is non-nil then
don't display the default-directory in a message."
  (interactive)
  (let ((dirname nil))
    (with-axiom-process-query-buffer
      (axiom-process-redirect-send-command ")cd ." (current-buffer))
      (goto-char (point-min))
      (let ((dirname-start (search-forward-regexp "default directory is[[:space:]]+" nil t))
            (dirname-end (progn
                           (search-forward-regexp "[[:blank:]]*$" nil t)
                           (match-beginning 0))))
        (when (and dirname-start dirname-end)
          (setq dirname (expand-file-name (file-name-as-directory (buffer-substring dirname-start dirname-end)))))
        (axiom-debug-message (format "CD: %S %S %S" dirname-start dirname-end dirname))))
    (when dirname
      (with-current-buffer axiom-process-buffer-name
        (setq default-directory dirname)
        (unless no-msg
          (message (format "Current directory now: %s" dirname)))))
    dirname))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluating a string
;;
(defun axiom-process-eval-string (str)
  "Evaluate the given string in the Axiom process."
  (if (null (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (progn
      (display-buffer axiom-process-buffer-name)
      (axiom-process-insert-command str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluating a region
;;
(defun axiom-process-eval-region (start end)
  "Evaluate the given region in the Axiom process."
  (interactive "r")
  (axiom-process-eval-string (buffer-substring-no-properties start end)))

(defun axiom-process-read-region (start end)
  "Copy region into a temporary file and )read it."
  (interactive "r")
  (if (null (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (progn
      (display-buffer axiom-process-buffer-name)
      (let ((tmp-filename (make-temp-file "axiom" nil ".input")))
        (write-region start end tmp-filename)
        (axiom-process-insert-command (format ")read %s" tmp-filename))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading and compiling files
;;
(defun axiom-process-read-file (filename &optional no-display)
  "Tell the Axiom process to read FILENAME.

If NO-DISPLAY is nil then also display the Axiom process buffer."
  (interactive (list (read-file-name "Read file: " nil nil nil (file-name-nondirectory (or (buffer-file-name) "")))
                     current-prefix-arg))
  (if (not (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (progn
      (unless no-display
        (display-buffer axiom-process-buffer-name))
      (axiom-process-insert-command (format ")read %s" (expand-file-name filename))))))

(defun axiom-process-compile-file (filename &optional no-display)
  "Tell the Axiom process to compile FILENAME.

If NO-DISPLAY is nil then display the Axiom compilation results
buffer, otherwise do not display it."
  (interactive (list (read-file-name "Compile file: " nil nil nil (file-name-nondirectory (or (buffer-file-name) "")))
                     current-prefix-arg))
  (if (not (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (let ((popup (unless no-display
                   (display-buffer (get-buffer-create axiom-process-compile-file-buffer-name)))))
      (with-current-buffer axiom-process-buffer-name
        (let ((current-dir (axiom-process-force-cd-update t))
              (result-dir (if axiom-process-compile-file-use-result-directory
                              (file-name-as-directory (expand-file-name axiom-process-compile-file-result-directory))
                            (file-name-directory (expand-file-name filename)))))
          (with-current-buffer (get-buffer-create axiom-process-compile-file-buffer-name)
            (setq buffer-read-only nil)
            (erase-buffer)
            (axiom-help-mode)
            (axiom-process-redirect-send-command (format ")cd %s" result-dir) (current-buffer) (not no-display))
            (axiom-process-redirect-send-command (format ")compile %s" (expand-file-name filename)) (current-buffer) (not no-display))
            (axiom-process-redirect-send-command (format ")cd %s" current-dir) (current-buffer) (not no-display))
            (set-buffer-modified-p nil)
            (setq buffer-read-only t))))
      (when (and popup axiom-select-popup-windows)
        (select-window popup)
        (end-of-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing/inspection utility functions
;;
(defun axiom-process-package-name (name-or-abbrev)
  (let ((rslt (assoc name-or-abbrev axiom-standard-package-info)))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun axiom-process-domain-name (name-or-abbrev)
  (let ((rslt (assoc name-or-abbrev axiom-standard-domain-info)))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun axiom-process-category-name (name-or-abbrev)
  (let ((rslt (assoc name-or-abbrev axiom-standard-category-info)))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun axiom-process-constructor-name (name-or-abbrev)
  (let ((rslt (or (assoc name-or-abbrev axiom-standard-package-info)
                  (assoc name-or-abbrev axiom-standard-domain-info)
                  (assoc name-or-abbrev axiom-standard-category-info))))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun axiom-process-verify-package-name-or-abbrev (name-or-abbrev)
  "Return package name if valid name or abbreviation, or nil otherwise."
  (let ((fquery (assoc name-or-abbrev axiom-standard-package-info))
        (rquery (rassoc name-or-abbrev axiom-standard-package-info)))
    (or (cdr fquery) (cdr rquery))))

(defun axiom-process-verify-domain-name-or-abbrev (name-or-abbrev)
  "Return domain name if valid name or abbreviation given, or nil otherwise."
  (let ((fquery (assoc name-or-abbrev axiom-standard-domain-info))
        (rquery (rassoc name-or-abbrev axiom-standard-domain-info)))
    (or (cdr fquery) (cdr rquery))))

(defun axiom-process-verify-category-name-or-abbrev (name-or-abbrev)
  "Return category name if valid name or abbreviation given, or nil otherwise."
  (let ((fquery (assoc name-or-abbrev axiom-standard-category-info))
        (rquery (rassoc name-or-abbrev axiom-standard-category-info)))
    (or (cdr fquery) (cdr rquery))))

(defun axiom-process-verify-constructor-name-or-abbrev (name-or-abbrev)
  (or (axiom-process-verify-package-name-or-abbrev name-or-abbrev)
      (axiom-process-verify-domain-name-or-abbrev name-or-abbrev)
      (axiom-process-verify-category-name-or-abbrev name-or-abbrev)))

(defun axiom-process-verify-operation-name (name)
  (car (member name axiom-standard-operation-info)))

(defun axiom-process-constructor-type (name-or-abbrev)
  (cond ((member name-or-abbrev axiom-standard-package-names)
         (cons :package :name))
        ((member name-or-abbrev axiom-standard-package-abbreviations)
         (cons :package :abbrev))
        ((member name-or-abbrev axiom-standard-domain-names)
         (cons :domain :name))
        ((member name-or-abbrev axiom-standard-domain-abbreviations)
         (cons :domain :abbrev))
        ((member name-or-abbrev axiom-standard-category-names)
         (cons :category :name))
        ((member name-or-abbrev axiom-standard-category-abbreviations)
         (cons :category :abbrev))
        (t
         (cons :constructor :unknown))))

(defun axiom-process-constructor-buffer-name (name-or-abbrev)
  (let ((ctype (car (axiom-process-constructor-type name-or-abbrev))))
    (format "*Axiom %s: %s*"
            (capitalize (subseq (symbol-name ctype) 1))
            (cond ((eq ctype :package)
                   (axiom-process-package-name name-or-abbrev))
                  ((eq ctype :domain)
                   (axiom-process-domain-name name-or-abbrev))
                  ((eq ctype :category)
                   (axiom-process-category-name name-or-abbrev))
                  (t
                   name-or-abbrev)))))

(defun axiom-process-show-constructor (name-or-abbrev &optional force-update)
  "Show information about NAME-OR-ABBREV in a popup buffer.

Works by calling ``)show NAME-OR-ABBREV'' in the Axiom process and
capturing its output.  When called interactively completion is
performed over all standard constructor names (packages, domains and
categories) and their abbreviations.

If the buffer already exists (from a previous call) then just switch
to it, unless FORCE-UPDATE is non-nil in which case the buffer is
reconstructed with another query to the Axiom process.

Interactively, FORCE-UPDATE can be set with a prefix argument."
  (interactive (list (completing-read
                      "Constructor: "
                      axiom-standard-constructor-names-and-abbreviations
                      nil 'confirm
                      (axiom-process-verify-constructor-name-or-abbrev (thing-at-point 'word)))
                     current-prefix-arg))
  (if (not (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (unless (equal "" name-or-abbrev)
      (let ((bufname (axiom-process-constructor-buffer-name name-or-abbrev)))
        (when (or (not (get-buffer bufname)) force-update)
          (with-current-buffer (get-buffer-create bufname)
            (setq buffer-read-only nil)
            (erase-buffer)
            (axiom-help-mode)
            (axiom-process-redirect-send-command (format ")show %s" name-or-abbrev) (current-buffer) t nil nil)
            (set-buffer-modified-p nil)
            (setq buffer-read-only t)))
        (let ((popup (display-buffer bufname)))
          (when (and popup axiom-select-popup-windows)
            (select-window popup)))))))

(defun axiom-process-show-package (name-or-abbrev &optional force-update)
  "Show information about NAME-OR-ABBREV in a popup buffer.

Works by calling ``)show NAME-OR-ABBREV'' in the Axiom process and
capturing its output.  When called interactively completion is
performed over all standard package names.

If the buffer already exists (from a previous call) then just switch
to it, unless FORCE-UPDATE is non-nil in which case the buffer is
reconstructed with another query to the Axiom process.

Interactively, FORCE-UPDATE can be set with a prefix argument."
  (interactive (list (completing-read
                      "Package: " axiom-standard-package-names-and-abbreviations nil 'confirm
                      (axiom-process-verify-package-name-or-abbrev (thing-at-point 'word)))
                     current-prefix-arg))
  (axiom-process-show-constructor name-or-abbrev force-update))

(defun axiom-process-show-domain (name-or-abbrev &optional force-update)
  "Show information about NAME-OR-ABBREV in a popup buffer.

Works by calling ``)show NAME-OR-ABBREV'' in the Axiom process and
capturing its output.  When called interactively completion is
performed over all standard domain names.

If the buffer already exists (from a previous call) then just switch
to it, unless FORCE-UPDATE is non-nil in which case the buffer is
reconstructed with another query to the Axiom process.

Interactively, FORCE-UPDATE can be set with a prefix argument."
  (interactive (list (completing-read
                      "Domain: " axiom-standard-domain-names-and-abbreviations nil 'confirm
                      (axiom-process-verify-domain-name-or-abbrev (thing-at-point 'word)))
                     current-prefix-arg))
  (axiom-process-show-constructor name-or-abbrev force-update))

(defun axiom-process-show-category (name-or-abbrev &optional force-update)
  "Show information about NAME-OR-ABBREV in a popup buffer.

Works by calling ``)show NAME-OR-ABBREV'' in the Axiom process and
capturing its output.  When called interactively completion is
performed over all standard category names.

If the buffer already exists (from a previous call) then just switch
to it, unless FORCE-UPDATE is non-nil in which case the buffer is
reconstructed with another query to the Axiom process.

Interactively, FORCE-UPDATE can be set with a prefix argument."
  (interactive (list (completing-read
                      "Category: " axiom-standard-category-names-and-abbreviations nil 'confirm
                      (axiom-process-verify-category-name-or-abbrev (thing-at-point 'word)))
                     current-prefix-arg))
  (axiom-process-show-constructor name-or-abbrev force-update))

(defun axiom-process-display-operation (operation-name &optional force-update)
  "Show information about OPERATION-NAME in a popup buffer.

Works by calling ``)display operation OPERATION-NAME'' in the Axiom
process and capturing its output.  When called interactively
completion is performed over all standard operation names.

If the buffer already exists (from a previous call) then just switch
to it, unless FORCE-UPDATE is non-nil in which case the buffer is
reconstructed with another query to the Axiom process.

Interactively, FORCE-UPDATE can be set with a prefix argument."
  (interactive (list (completing-read
                      "Operation: " axiom-standard-operation-names nil 'confirm
                      (axiom-process-verify-operation-name (thing-at-point 'word)))
                     current-prefix-arg))
  (if (not (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (unless (equal "" operation-name)
      (let ((bufname (format "*Axiom Operation: %s*" operation-name)))
        (when (or (not (get-buffer bufname)) force-update)
          (with-current-buffer (get-buffer-create bufname)
            (setq buffer-read-only nil)
            (erase-buffer)
            (axiom-help-mode)
            (axiom-process-redirect-send-command (format ")display operation %s" operation-name) (current-buffer) t nil nil)
            (set-buffer-modified-p nil)
            (setq buffer-read-only t)))
        (let ((popup (display-buffer bufname)))
          (when (and popup axiom-select-popup-windows)
            (select-window popup)))))))

(defun axiom-process-apropos-thing-at-point (name &optional is-constructor)
  "Show information about NAME in a popup buffer.

When called interactively NAME defaults to the word around point, and
completion is performed over all standard constructor and operation
names.

If NAME is a standard constructor name then call ``)show NAME'' in the
Axiom process and capture its output, otherwise assume it's an
operation name and call ``)display operation NAME'' instead.  This can
be overridden by setting IS-CONSTRUCTOR non-nil, in which case ``)show
NAME'' will always be called.  Interactively this can be done with a
prefix argument."
  (interactive (list (completing-read "Apropos: " axiom-standard-names-and-abbreviations
                                      nil 'confirm (thing-at-point 'word))
                     current-prefix-arg))
  (if (not (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (unless (equal "" name)
      (cond ((or (member name axiom-standard-constructor-names-and-abbreviations) is-constructor)
             (axiom-process-show-constructor name t))
            (t
             (axiom-process-display-operation name t))))))

(defun axiom-process-webview-constructor (name-or-abbrev)
  "Show information about NAME-OR-ABBREV in a web browser.

Invokes `browse-url' on a URL made by appending the given
constructor name and .html to the base URL held in customizable
variable `axiom-process-webview-url'."
  (interactive (list (completing-read
                      "Show web-page for constructor: " axiom-standard-constructor-names-and-abbreviations nil 'confirm
                      (axiom-process-verify-constructor-name-or-abbrev (thing-at-point 'word)))))
  (let ((url (concat axiom-process-webview-url
                     (axiom-process-constructor-name name-or-abbrev)
                      ".html")))
    (browse-url url)))

(defun axiom-process-edit-constructor-source (name-or-abbrev)
  "Open the SPAD source file containing NAME-OR-ABBREV."
  (interactive (list (completing-read
                      "Find source for constructor: "
                      axiom-standard-constructor-names-and-abbreviations
                      nil 'confirm
                      (axiom-process-verify-constructor-name-or-abbrev (thing-at-point 'word)))))
  (let ((location (axiom-process-find-constructor-source name-or-abbrev)))
    (if location
	(let ((buf (find-file (first location))))
	  (switch-to-buffer buf)
	  (goto-line (second location)))
      (message "Source not found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Axiom process mode -- derived from COMINT mode
;;
(defvar axiom-process-package-face  'axiom-package-name)
(defvar axiom-process-domain-face   'axiom-domain-name)
(defvar axiom-process-category-face 'axiom-category-name)

(defvar axiom-process-font-lock-keywords
  (list (cons axiom-standard-package-names-regexp          'axiom-process-package-face)
        (cons axiom-standard-package-abbreviations-regexp  'axiom-process-package-face)
        (cons axiom-standard-domain-names-regexp           'axiom-process-domain-face)
        (cons axiom-standard-domain-abbreviations-regexp   'axiom-process-domain-face)
        (cons axiom-standard-category-names-regexp         'axiom-process-category-face)
        (cons axiom-standard-category-abbreviations-regexp 'axiom-process-category-face)))

(define-derived-mode axiom-process-mode comint-mode "Axiom Process"
  "Major mode for interaction with a running Axiom process."
  :group 'axiom
  (setq comint-prompt-regexp (concat "\\(" axiom-process-prompt-regexp
                                     "\\|" axiom-process-break-prompt-regexp "\\)"))
  (setq comint-get-old-input (function axiom-process-get-old-input))
  (setq font-lock-defaults (list axiom-process-font-lock-keywords))
  (setq axiom-menu-read-file-enable t)
  (setq axiom-menu-compile-file-enable t)
  (let ((schedule-cd-update nil)
        (process-buffer (current-buffer)))
    (add-hook 'comint-input-filter-functions
              (lambda (str)  ; lexical closure
                (when (or (string-match "^)cd" str)
                          (string-match "^)read" str))
                  (setq schedule-cd-update t))
                str))
    (add-hook 'comint-output-filter-functions
              (lambda (str)  ; lexical closure
                (when (and (string-match axiom-process-prompt-regexp str)
                           schedule-cd-update)
                  (setq schedule-cd-update nil)
                  (let ((axiom-process-buffer-name process-buffer))  ; dynamic binding
                    (axiom-process-force-cd-update)))))
    (unless (equal "" axiom-process-preamble)
      (axiom-process-insert-command axiom-process-preamble))
    (setq schedule-cd-update t)
    (while schedule-cd-update
      (sit-for 1))))

(defun axiom-process-start (process-cmd)
  "Start an Axiom process in a buffer.

The name of the buffer is given by variable
`axiom-process-buffer-name', and uses major mode
`axiom-process-mode'.  Return the buffer in which the process is
started.  If there is a process already running then simply
return it."
  (with-current-buffer (get-buffer-create axiom-process-buffer-name)
    (when (not (comint-check-proc (current-buffer)))
      (let ((cmdlist (split-string process-cmd)))
        (apply (function make-comint)
               (substring axiom-process-buffer-name 1 -1)
               (car cmdlist) nil (cdr cmdlist)))
      (axiom-process-mode))
    (current-buffer)))

(defun run-axiom (cmd)
  "Run an Axiom process in a buffer using program command line CMD.

The name of the buffer is given by variable
`axiom-process-buffer-name', and uses major mode `axiom-process-mode'.
With a prefix argument, allow CMD to be edited first (default is value
of `axiom-process-program').  If there is a process already running
then simply switch to it."
  (interactive (list (if current-prefix-arg
                         (read-string "Run Axiom: " axiom-process-program)
                       axiom-process-program)))
  (let ((buf (axiom-process-start cmd)))
    (pop-to-buffer buf)))

(provide 'axiom-process-mode)
