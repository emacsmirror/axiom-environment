;;; axiom-selector.el --- A buffer selector utility for the Axiom environment -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2020 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A utility for quickly selecting a buffer from the Axiom environment.

;; Inspired by (and code borrowed from) the Slime selector function.

;;; Code:

(require 'cl-lib)

(defcustom axiom-selector-help-buffer-name "*Axiom Selector Help*"
  "Axiom selector help buffer name."
  :type 'string
  :group 'axiom)

(defvar axiom-selector-functions nil
  "List of functions for the `axiom-selector' function.

Each element is a list (KEY DESCRIPTION FUNCTION), where
DESCRIPTION is a one-line description of the command.")

;;;###autoload
(defun axiom-selector ()
  "Invoke a selector function by entering a single character.

The user is prompted for a single character indicating the
desired function. The `?' character describes the available
functions.  See `define-axiom-selector-function' for defining new
functions."
  (interactive)
  (message "Select [%s]: "
           (apply #'string (mapcar #'car axiom-selector-functions)))
  (let* ((ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (selector-entry (assq ch axiom-selector-functions)))
    (cond ((null selector-entry)
           (message "No function for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (axiom-selector))
          (t
           (funcall (cl-third selector-entry))))))

(defmacro define-axiom-selector-function (key description &rest body)
  "Define a new `axiom-selector' function.

KEY is the key the user will enter to choose this function.
DESCRIPTION is a one-line sentence describing the function.
BODY is a series of forms which are evaluated when the command
is chosen."
  (let ((function `(lambda ()
                     (progn ,@body))))
    `(progn
       (setq axiom-selector-functions
             (cons (list ,key ,description ,function)
                   (assq-delete-all ,key axiom-selector-functions))))))

(define-axiom-selector-function ?? "Axiom selector help"
  (ignore-errors (kill-buffer axiom-selector-help-buffer-name))
  (with-current-buffer (get-buffer-create axiom-selector-help-buffer-name)
    (insert "Selector Methods:\n\n")
    (dolist (entry axiom-selector-functions)
      (insert (format "%c:\t%s\n" (cl-first entry) (cl-second entry))))
    (help-mode)
    (display-buffer (current-buffer) nil t)
    (shrink-window-if-larger-than-buffer 
     (get-buffer-window (current-buffer))))
  (axiom-selector)
  (current-buffer))

(define-axiom-selector-function ?q "Quit selector"
  (let ((help-buffer (get-buffer axiom-selector-help-buffer-name)))
    (when help-buffer
      (delete-window (get-buffer-window help-buffer))
      (kill-buffer help-buffer))))

(define-axiom-selector-function ?r "Switch to Axiom REPL buffer"
  (let ((buf (get-buffer axiom-process-buffer-name)))
    (if buf
        (switch-to-buffer buf)
      (message "Axiom REPL not available, try M-x run-axiom"))))

(defun axiom-find-recent-buffer (mode)
  (let ((bufs (buffer-list (window-frame nil)))
        (buf nil))
    (while (and bufs (null buf))
      (save-excursion
        (with-current-buffer (cl-first bufs)
          (when (eql major-mode mode)
            (setq buf (current-buffer)))))
      (setq bufs (cl-rest bufs)))
    buf))

(define-axiom-selector-function ?i "Switch to most recent Axiom Input buffer"
  (let ((buf (axiom-find-recent-buffer 'axiom-input-mode)))
    (if buf
        (switch-to-buffer buf)
      (message "No Axiom Input buffer found"))))

(define-axiom-selector-function ?s "Switch to most recent Axiom SPAD buffer"
  (let ((buf (axiom-find-recent-buffer 'axiom-spad-mode)))
    (if buf
        (switch-to-buffer buf)
      (message "No Axiom SPAD buffer found"))))

(define-axiom-selector-function ?b "List Axiom buffers"
  (axiom-buffer-menu))

(define-axiom-selector-function ?a "List all Axiom buffers"
  (axiom-buffer-menu))

(provide 'axiom-selector)

;;; axiom-selector.el ends here
