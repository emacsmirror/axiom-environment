;;; axiom-process-mode.el --- A Comint-derived mode for Axiom -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2019 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A mode for interacting with a running Axiom process.

;;; Code:

(require 'cl-lib)
(require 'axiom-base)
(require 'axiom-help-mode)
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

(defcustom axiom-process-prompt-regexp "^.*(\\([[:digit:]]+\\|NIL\\)) ->\\|^->"
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
  "Directory in which to place compiled object files.

Only used when variable
`axiom-process-compile-file-use-result-directory' is non-NIL."
  :type 'string
  :group 'axiom)

(defcustom axiom-process-compile-file-use-result-directory nil
  "Non-nil to place compilation results in a central directory.

When non-nil place compiled object files in the directory named
by variable `axiom-process-compile-file-result-directory',
otherwise they will be placed in the same directory as the source
file."
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

(defvar axiom-process-mode-syntax-table
  (copy-syntax-table axiom-common-syntax-table)
  "The Axiom process mode syntax table.")

(defvar axiom-process-mode-map
  (let ((map (copy-keymap axiom-common-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-c") 'comint-interrupt-subjob)
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

;;;###autoload
(defun axiom-process-redirect-send-command (command output-buffer &optional display echo-cmd echo-result
                                                    op-cmd op-prompt)
  "Send COMMAND to Axiom and put result in OUTPUT-BUFFER.

If DISPLAY is non-nil then display the result buffer.

If ECHO-CMD is non-nil then copy the command to the process buffer,
and if ECHO-RESULT is non-nil then also copy the result too.

If OP-CMD is non-nil then include command in output to
OUTPUT-BUFFER.  If OP-PROMPT is non-nil then also include
prompt in output to OUTPUT-BUFFER."
  (with-current-buffer axiom-process-buffer-name
    (let ((proc (get-buffer-process (current-buffer))))
      (when op-prompt
        (let* ((real-bol (+ (point) (save-excursion (skip-chars-backward "^\n"))))
               (prompt (buffer-substring-no-properties real-bol (point))))
          (with-current-buffer output-buffer
            (insert prompt))))
      (when op-cmd
        (with-current-buffer output-buffer
          (insert command "\n")))
      (when echo-cmd
        (goto-char (process-mark proc))
        (insert-before-markers command "\n"))
      (comint-redirect-send-command command output-buffer echo-result (not display))
      (while (not comint-redirect-completed)
        (accept-process-output proc)
        (redisplay))
      (when (and echo-cmd (not echo-result))  ; get prompt back
        (axiom-process-insert-command "")))))

(defun axiom-process-get-old-input ()
  "An Axiom-specific replacement for `comint-get-old-input'.

Return the concatenation of the current line and all subsequent
continuation-lines (underscores escape new lines)."
  (comint-bol)
  (axiom-get-rest-of-line))

;;;###autoload
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
(defun axiom-process-eval-string (str &optional no-display)
  "Evaluate the given string in the Axiom process."
  (if (null (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (unless no-display
      (let ((win (display-buffer axiom-process-buffer-name nil t)))
        (when axiom-select-displayed-repl
          (select-window win))))
    (axiom-process-insert-command str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluating a region
;;
;;;###autoload
(defun axiom-process-eval-region (start end &optional no-display)
  "Evaluate the given region in the Axiom process."
  (interactive "r\nP")
  (axiom-process-eval-string (buffer-substring-no-properties start end no-display)))

;;;###autoload
(defun axiom-process-read-region (start end &optional no-display)
  "Copy region into a temporary file and )read it."
  (interactive "r\nP")
  (if (null (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (let ((tmp-filename (make-temp-file "axiom" nil ".input")))
      (write-region start end tmp-filename)
      (unless no-display
        (let ((win (display-buffer axiom-process-buffer-name nil t)))
          (when axiom-select-displayed-repl
            (select-window win))))
      (axiom-process-insert-command (format ")read %s" tmp-filename)))))

(defun axiom-process-read-pile (&optional no-display)
  (interactive "P")
  (let ((start (point))
        (end (point)))
    (save-excursion
      (beginning-of-line)
      (while (and (not (eql (point) (point-min)))
                  (member (char-after) (list 9 10 12 13 32)))
        (forward-line -1))
      (setq start (point)))
    (save-excursion
      (beginning-of-line)
      (forward-line +1)
      (while (and (not (eql (point) (point-max)))
                  (member (char-after) (list 9 10 12 13 32)))
        (forward-line +1))
      (setq end (point)))
    (axiom-flash-region start end)
    (axiom-process-read-region start end no-display)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading and compiling files
;;
;;;###autoload
(defun axiom-process-read-file (filename &optional no-display)
  "Tell the Axiom process to read FILENAME.

If NO-DISPLAY is nil then also display the Axiom process buffer."
  (interactive (list (read-file-name "Read file: " nil nil nil (file-name-nondirectory (or (buffer-file-name) "")))
                     current-prefix-arg))
  (if (not (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (progn
      (unless no-display
        (let ((win (display-buffer axiom-process-buffer-name nil t)))
          (when axiom-select-displayed-repl
            (select-window win))))
      (axiom-process-insert-command (format ")read %s" (expand-file-name filename))))))

;;;###autoload
(defun axiom-process-read-buffer (&optional no-display)
  "Read the current buffer into the Axiom process.

If NO-DISPLAY is nil then also display the Axiom process buffer."
  (interactive "P")
  (let ((file (if (and (buffer-file-name)
                       (not (buffer-modified-p)))
                  (buffer-file-name)
                (let ((tmp-file (make-temp-file "axiom" nil ".input")))
                  (write-region (point-min) (point-max) tmp-file)
                  tmp-file))))
    (axiom-process-read-file file no-display)))

;;;###autoload
(defun axiom-process-compile-file (filename &optional no-display)
  "Tell the Axiom process to compile FILENAME.

If NO-DISPLAY is nil then display the Axiom compilation results
buffer, otherwise do not display it."
  (interactive (list (read-file-name "Compile file: " nil nil nil (file-name-nondirectory (or (buffer-file-name) "")))
                     current-prefix-arg))
  (if (not (get-buffer axiom-process-buffer-name))
      (message axiom-process-not-running-message)
    (with-current-buffer axiom-process-buffer-name
      (let ((current-dir (axiom-process-force-cd-update t))
            (result-dir (if axiom-process-compile-file-use-result-directory
                            (file-name-as-directory (expand-file-name axiom-process-compile-file-result-directory))
                          (file-name-directory (expand-file-name filename)))))
        (with-current-buffer (get-buffer-create axiom-process-compile-file-buffer-name)
          (setq buffer-read-only nil)
          (erase-buffer)
          (axiom-help-mode)
          (unless no-display
            (display-buffer axiom-process-compile-file-buffer-name nil t)
            (redisplay t))
          (axiom-process-redirect-send-command (format ")cd %s" result-dir) (current-buffer) (not no-display))
          (axiom-process-redirect-send-command (format ")compile %s" (expand-file-name filename)) (current-buffer) (not no-display))
          (axiom-process-redirect-send-command (format ")cd %s" current-dir) (current-buffer) (not no-display))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t))))
      (when (and axiom-select-displayed-repl (not no-display))
        (select-window (display-buffer axiom-process-compile-file-buffer-name nil t)))))

;;;###autoload
(defun axiom-process-compile-buffer (&optional no-display)
  "Compile the current buffer in the Axiom process.

If NO-DISPLAY is nil then display the Axiom compilation results
buffer, otherwise do not display it."
  (interactive "P")
  (let ((file (if (and (buffer-file-name)
                       (not (buffer-modified-p)))
                  (buffer-file-name)
                (let ((tmp-file (make-temp-file "axiom" nil ".spad")))
                  (write-region (point-min) (point-max) tmp-file)
                  tmp-file))))
    (axiom-process-compile-file file no-display)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing/inspection utility functions
;;
(defun axiom-process-package-name (name-or-abbrev)
  (let ((rslt (assoc name-or-abbrev axiom-standard-package-info)))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun axiom-process-package-abbrev (name-or-abbrev)
  (let ((rslt (rassoc name-or-abbrev axiom-standard-package-info)))
    (if rslt
        (car rslt)
      name-or-abbrev)))

(defun axiom-process-domain-name (name-or-abbrev)
  (let ((rslt (assoc name-or-abbrev axiom-standard-domain-info)))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun axiom-process-domain-abbrev (name-or-abbrev)
  (let ((rslt (rassoc name-or-abbrev axiom-standard-domain-info)))
    (if rslt
        (car rslt)
      name-or-abbrev)))

(defun axiom-process-category-name (name-or-abbrev)
  (let ((rslt (assoc name-or-abbrev axiom-standard-category-info)))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun axiom-process-category-abbrev (name-or-abbrev)
  (let ((rslt (rassoc name-or-abbrev axiom-standard-category-info)))
    (if rslt
        (car rslt)
      name-or-abbrev)))

(defun axiom-process-constructor-name (name-or-abbrev)
  (let ((rslt (or (assoc name-or-abbrev axiom-standard-package-info)
                  (assoc name-or-abbrev axiom-standard-domain-info)
                  (assoc name-or-abbrev axiom-standard-category-info))))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun axiom-process-constructor-abbrev (name-or-abbrev)
  (let ((rslt (or (rassoc name-or-abbrev axiom-standard-package-info)
                  (rassoc name-or-abbrev axiom-standard-domain-info)
                  (rassoc name-or-abbrev axiom-standard-category-info))))
    (if rslt
        (car rslt)
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
            (capitalize (cl-subseq (symbol-name ctype) 1))
            (cond ((eq ctype :package)
                   (axiom-process-package-name name-or-abbrev))
                  ((eq ctype :domain)
                   (axiom-process-domain-name name-or-abbrev))
                  ((eq ctype :category)
                   (axiom-process-category-name name-or-abbrev))
                  (t
                   name-or-abbrev)))))

(defun axiom-process-display-thing ()
  (interactive)
  (let ((name (thing-at-point 'word)))
    (if (not (get-buffer axiom-process-buffer-name))
        (message axiom-process-not-running-message)
      (unless (equal "" name)
        (cond ((member name axiom-standard-constructor-names-and-abbreviations)
               (axiom-process-show-constructor name))
              (t
               (axiom-process-display-operation name)))))))

(defvar axiom-process-clickable-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'axiom-process-display-thing)
    (define-key map [mouse-2] 'axiom-process-display-thing)
    map)
  "Keymap for clickable items in an Axiom Help mode buffer.")

(defun axiom-process-make-clickable (begin end tooltip-text)
  (add-text-properties begin end
                       (list 'mouse-face 'highlight
                             'help-echo tooltip-text
                             'keymap axiom-process-clickable-map
                             'follow-link 'mouse-face)))

(defun axiom-process-make-all-clickables ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[[:word:]]+" nil t)
      (let* ((word (match-string-no-properties 0))
             (info (cond ((member word axiom-standard-package-names-and-abbreviations)
                          (cons t (concat (axiom-process-package-abbrev word) " = "
                                          (axiom-process-package-name word) " [P]")))
                         ((member word axiom-standard-domain-names-and-abbreviations)
                          (cons t (concat (axiom-process-domain-abbrev word) " = "
                                          (axiom-process-domain-name word) " [D]")))
                         ((member word axiom-standard-category-names-and-abbreviations)
                          (cons t (concat (axiom-process-category-abbrev word) " = "
                                          (axiom-process-category-name word) " [C]")))
                         ((member word axiom-standard-operation-names)
                          (cons t nil)))))
        (when (car info)
          (axiom-process-make-clickable (match-beginning 0) (match-end 0) (cdr info)))))))

(defun axiom-process-document-constructor (name-or-abbrev &optional force-update)
  "Construct a buffer containing documentation for NAME-OR-ABBREV."
  (if (not (get-buffer axiom-process-buffer-name))
      (progn (message axiom-process-not-running-message) nil)
    (unless (equal "" name-or-abbrev)
      (let ((bufname (axiom-process-constructor-buffer-name name-or-abbrev)))
        (when (or (not (get-buffer bufname)) force-update)
          (with-current-buffer (get-buffer-create bufname)
            (setq buffer-read-only nil)
            (erase-buffer)
            (axiom-help-mode)
            (axiom-process-redirect-send-command (format ")show %s" name-or-abbrev) (current-buffer) t nil nil)
            (axiom-process-make-all-clickables)
            (set-buffer-modified-p nil)
            (setq buffer-read-only t)))
        (get-buffer bufname)))))

;;;###autoload
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
  (let ((buf (axiom-process-document-constructor name-or-abbrev force-update)))
    (when buf
      (let ((popup (display-buffer buf nil t)))
        (when (and popup axiom-select-popup-windows)
          (select-window popup))))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

(defun axiom-process-document-operation (operation-name &optional force-update)
  "Create a buffer containing documentation for OPERATION-NAME."
  (if (not (get-buffer axiom-process-buffer-name))
      (progn (message axiom-process-not-running-message) nil)
    (unless (equal "" operation-name)
      (let ((bufname (format "*Axiom Operation: %s*" operation-name)))
        (when (or (not (get-buffer bufname)) force-update)
          (with-current-buffer (get-buffer-create bufname)
            (setq buffer-read-only nil)
            (erase-buffer)
            (axiom-help-mode)
            (axiom-process-redirect-send-command (format ")display operation %s" operation-name) (current-buffer) t nil nil)
            (axiom-process-make-all-clickables)
            (set-buffer-modified-p nil)
            (setq buffer-read-only t)))
        (get-buffer bufname)))))

;;;###autoload
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
  (let ((buf (axiom-process-document-operation operation-name force-update)))
    (when buf
      (let ((popup (display-buffer buf nil t)))
        (when (and popup axiom-select-popup-windows)
          (select-window popup))))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun axiom-process-edit-constructor-source (name-or-abbrev)
  "Open the SPAD source file containing NAME-OR-ABBREV."
  (interactive (list (completing-read
                      "Find source for constructor: "
                      axiom-standard-constructor-names-and-abbreviations
                      nil 'confirm
                      (axiom-process-verify-constructor-name-or-abbrev (thing-at-point 'word)))))
  (let ((location (axiom-process-find-constructor-source name-or-abbrev)))
    (if location
	(let ((buf (find-file (cl-first location))))
	  (switch-to-buffer buf)
          (goto-char (point-min))
          (forward-line (cl-second location)))
      (message "Source not found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-completion functions
;;
(defun axiom-process-list-filenames (dir &optional filter)
  (with-current-buffer axiom-process-buffer-name
    (let* ((absolute-dir (cond ((null dir)
                                default-directory)
                               ((not (file-name-absolute-p dir))
                                (concat default-directory dir))
                               (t
                                dir)))
           (dir-files (directory-files absolute-dir dir))
           (subdirs nil)
           (subfiles nil))
      (dolist (file dir-files)
        (if (file-directory-p file)
            (push (file-name-as-directory (file-name-nondirectory file)) subdirs)
          (push (file-name-nondirectory file) subfiles)))
      (cond ((eql filter :dirs)
             subdirs)
            ((eql filter :files)
             subfiles)
            (t
             (append subdirs subfiles))))))

(defun axiom-process-complete-command-filename (&optional filter)
  (let ((partial-start nil)
        (partial-end nil)
        (line-end nil))
    (save-excursion
      (setq partial-end (point))
      (end-of-line)
      (setq line-end (point))
      (beginning-of-line)
      (setq partial-start (search-forward-regexp ")[[:word:]]+[[:blank:]]+" line-end t))
      (when partial-start
        (when (> partial-start partial-end)
          (setq partial-start partial-end))
        (let* ((partial (buffer-substring-no-properties partial-start partial-end))
               (dir-path (file-name-directory partial))
               (file-prefix (file-name-nondirectory partial))
               (partial-split (- partial-end (length file-prefix))))
          (list partial-split
                partial-end
                (axiom-process-list-filenames dir-path filter)))))))

(defun axiom-process-complete-command-line ()
  (let ((filter nil))
    (save-excursion
      (beginning-of-line)
      (setq filter (cond ((looking-at "[[:blank:]]*)cd[[:blank:]]+")
                          :dirs)
                         ((looking-at "[[:blank:]]*)read[[:blank:]]+")
                          :all)
                         ((looking-at "[[:blank:]]*)compile[[:blank:]]+")
                          :all)
                         ((looking-at "[[:blank:]]*)library[[:blank:]]+")
                          :all))))
    (and filter (axiom-process-complete-command-filename filter))))

(defun axiom-process-complete-symbol ()
  (and (looking-back "[[:word:]]+" nil t)
       (list (match-beginning 0)
             (match-end 0)
             axiom-standard-names-and-abbreviations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indenting functions
;;
(defun axiom-process-is-command-line ()
  (save-excursion     
    (beginning-of-line)
    (looking-at "[[:blank:]]*)[[:word:]]+[[:blank:]]+")))

(defun axiom-process-interactive-complete ()
  (interactive)
  (if (and (boundp 'company-mode) company-mode)
      (company-complete)
    (complete-symbol nil)))

(defun axiom-process-indent-line ()
  (if (or (axiom-process-is-command-line)
          (eql (char-syntax (char-before)) ?w))
      (axiom-process-interactive-complete)
    (indent-relative-maybe)))

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

;;;###autoload
(define-derived-mode axiom-process-mode comint-mode "Axiom Process"
  "Major mode for interaction with a running Axiom process."
  :group 'axiom
  (setq comint-prompt-regexp (concat "\\(" axiom-process-prompt-regexp
                                     "\\|" axiom-process-break-prompt-regexp "\\)"))
  (setq comint-get-old-input (function axiom-process-get-old-input))
  (setq font-lock-defaults (list axiom-process-font-lock-keywords))
  (setq electric-indent-inhibit t)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'completion-at-point-functions)
  (make-local-variable 'comint-input-filter-functions)
  (make-local-variable 'comint-output-filter-functions)
  (setq indent-line-function 'axiom-process-indent-line)
  (setq completion-at-point-functions '(axiom-process-complete-command-line
                                        axiom-process-complete-symbol))
  (setq axiom-menu-compile-buffer-enable nil)
  (setq axiom-menu-compile-file-enable t)
  (setq axiom-menu-read-buffer-enable nil)
  (setq axiom-menu-read-file-enable t)
  (setq axiom-menu-read-region-enable t)
  (setq axiom-menu-read-pile-enable nil)
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

;;;###autoload
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

;;; axiom-process-mode.el ends here
