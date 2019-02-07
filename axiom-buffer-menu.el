;;; axiom-buffer-menu.el --- Display a list of Axiom buffers -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2017 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;;; Code:

(defcustom axiom-buffer-menu-bufname "*Axiom Buffer Menu*"
  "Name of the buffer in which to display the buffer-menu."
  :type 'string
  :group 'axiom)

(defcustom axiom-buffer-menu-startcolumn-bufprop 0
  "Starting column from which to display buffer properties."
  :type 'integer
  :group 'axiom)

(defcustom axiom-buffer-menu-startcolumn-bufname 3
  "Starting column from which to display buffer name."
  :type 'integer
  :group 'axiom)

(defcustom axiom-buffer-menu-startcolumn-bufpath 36
  "Starting column from which to display buffer path."
  :type 'integer
  :group 'axiom)

(defface axiom-buffer-menu-group-heading '((t :weight bold))
  "Face used for displaying Axiom Buffer Menu group headings"
  :group 'axiom)

(defvar axiom-buffer-menu-invoking-buffer nil
  "Buffer from which ``axiom-buffer-menu'' was invoked.")

(defvar axiom-buffer-menu-startpoint-input 0
  "Starting point of Input buffer list display.")

(defvar axiom-buffer-menu-startpoint-spad 0
  "Starting point of SPAD buffer list display.")

(defvar axiom-buffer-menu-startpoint-help 0
  "Starting point of Help buffer list display.")

(defvar axiom-buffer-menu-startpoint-cursor 0
  "Starting point of cursor in Axiom Buffer Menu buffer.")

(defvar axiom-buffer-menu-mode-map
  (let ((map (make-sparse-keymap "Axiom")))
    (suppress-keymap map t)
    (define-key map (kbd "<tab>") 'axiom-buffer-menu-cycle-groups-forward)
    (define-key map (kbd "TAB") 'axiom-buffer-menu-cycle-groups-forward)
    (define-key map (kbd "<backtab>") 'axiom-buffer-menu-cycle-groups-backward)
    (define-key map (kbd "S-<tab>") 'axiom-buffer-menu-cycle-groups-backward)
    (define-key map (kbd "M-TAB") 'axiom-buffer-menu-cycle-groups-backward)
    (define-key map (kbd "C-g") 'axiom-buffer-menu-quit)
    (define-key map (kbd "q") 'axiom-buffer-menu-quit)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "SPC") 'axiom-buffer-menu-cycle-forward)
    (define-key map (kbd "b") 'axiom-buffer-menu-cycle-backward)
    (define-key map (kbd "v") 'axiom-buffer-menu-select)
    (define-key map (kbd "RET") 'axiom-buffer-menu-select)
    (define-key map (kbd "<mouse-1>") 'axiom-buffer-menu-mouse-select)
    (define-key map (kbd "o") 'axiom-buffer-menu-select-other-window)
    (define-key map (kbd "K") 'axiom-buffer-menu-kill-buffer)
    (define-key map (kbd "B") 'axiom-buffer-menu-bury-buffer)
    map)
  "The Axiom Buffer Menu local keymap.")

(defun axiom-buffer-menu-quit ()
  (interactive)
  (quit-window))

(defun axiom-buffer-menu-get-bufname ()
  "Return name of buffer described by current line of buffer-menu."
  (save-excursion
    (beginning-of-line)
    (forward-char axiom-buffer-menu-startcolumn-bufname)
    (get-text-property (point) 'buffer-name)))

(defun axiom-buffer-menu-kill-buffer ()
  "Kill this line's buffer."
  (interactive)
  (let ((selected-buffer (axiom-buffer-menu-get-bufname))
        (menu-location (point)))
    (cond (selected-buffer
           (kill-buffer selected-buffer)
           (axiom-buffer-menu-prepare-buffer)
           (switch-to-buffer axiom-buffer-menu-bufname)
           (goto-char menu-location))
          (t
           (message "No buffer on this line")))))

(defun axiom-buffer-menu-bury-buffer ()
  "Bury this line's buffer."
  (interactive)
  (let ((selected-buffer (axiom-buffer-menu-get-bufname))
        (menu-location (point)))
    (cond (selected-buffer
           (bury-buffer selected-buffer)
           (axiom-buffer-menu-prepare-buffer)
           (switch-to-buffer axiom-buffer-menu-bufname)
           (goto-char menu-location))
          (t
           (message "No buffer on this line")))))

(defun axiom-buffer-menu-select ()
  "Select this line's buffer in this window."
  (interactive)
  (let ((selected-buffer (axiom-buffer-menu-get-bufname)))
    (cond (selected-buffer
           (switch-to-buffer selected-buffer)
           (kill-buffer axiom-buffer-menu-bufname))
          (t
           (message "No buffer on this line")))))

(defun axiom-buffer-menu-select-other-window ()
  "Display this line's buffer in another window."
  (interactive)
  (let ((selected-buffer (axiom-buffer-menu-get-bufname)))
    (cond (selected-buffer
           (display-buffer selected-buffer '(display-buffer-reuse-window (inhibit-same-window . t))))
          (t
           (message "No buffer on this line")))))

(defun axiom-buffer-menu-mouse-select (event)
  "Select the buffer whose line you click on."
  (interactive "e")
  (let (buffer)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq buffer (axiom-buffer-menu-get-bufname))))
    (select-window (posn-window (event-end event)))
    (switch-to-buffer buffer)))

(defun axiom-buffer-menu-cycle-forward ()
  "Move to next active line of buffer-menu."
  (interactive)
  (let ((newpoint nil))
    (save-excursion
      (let ((keep-searching t))
        (while keep-searching
          (if (> (forward-line 1) 0)
              (setq keep-searching nil)
            (when (axiom-buffer-menu-get-bufname)
              (setq keep-searching nil)
              (setq newpoint (point)))))))
    (when newpoint
      (goto-char newpoint))))

(defun axiom-buffer-menu-cycle-backward ()
  "Move to previous active line of buffer-menu."
  (interactive)
  (let ((newpoint nil))
    (save-excursion
      (let ((keep-searching t))
        (while keep-searching
          (if (< (forward-line -1) 0)
              (setq keep-searching nil)
            (when (axiom-buffer-menu-get-bufname)
              (setq keep-searching nil)
              (setq newpoint (point)))))))
    (when newpoint
      (goto-char newpoint))))

(defun axiom-buffer-menu-cycle-groups-forward ()
  "Move cursor to start of next buffer group."
  (interactive)
  (cond ((< (point) axiom-buffer-menu-startpoint-input)
         (goto-char axiom-buffer-menu-startpoint-input))
        ((< (point) axiom-buffer-menu-startpoint-spad)
         (goto-char axiom-buffer-menu-startpoint-spad))
        ((< (point) axiom-buffer-menu-startpoint-help)
         (goto-char axiom-buffer-menu-startpoint-help))
        (t
         (goto-char axiom-buffer-menu-startpoint-input))))

(defun axiom-buffer-menu-cycle-groups-backward ()
  "Move cursor to start of previous buffer group."
  (interactive)
  (cond ((> (point) axiom-buffer-menu-startpoint-help)
         (goto-char axiom-buffer-menu-startpoint-help))
        ((> (point) axiom-buffer-menu-startpoint-spad)
         (goto-char axiom-buffer-menu-startpoint-spad))
        ((> (point) axiom-buffer-menu-startpoint-input)
         (goto-char axiom-buffer-menu-startpoint-input))
        (t
         (goto-char axiom-buffer-menu-startpoint-help))))

(defun axiom-buffer-menu-make-truncated-name (name max-length)
  "Construct printed name, truncating if necessary."
  (let ((left-trunc-length 12))
    (cond ((or (null max-length) (<= (length name) max-length))
           name)
          (t
           (concat (substring name 0 left-trunc-length)
                   "..."
                   (substring name (- (length name)
                                      (- max-length (+ left-trunc-length 3)))))))))

(defun axiom-buffer-menu-prepare-buffer ()
  "Setup the Axiom Buffer Menu buffer."
  (with-current-buffer (get-buffer-create axiom-buffer-menu-bufname)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq standard-output (current-buffer))
    ;; Record the column where buffer names start.
    (dolist (show-type (list :input :spad :help))
      (let ((heading-startpoint (point)))
        (cond ((eql show-type :input)
               (princ "INPUT BUFFERS\n")
               (put-text-property heading-startpoint (point) 'face 'axiom-buffer-menu-group-heading)
               (setq axiom-buffer-menu-startpoint-input (point))
               (setq axiom-buffer-menu-startpoint-cursor (point)))
              ((eql show-type :spad)
               (princ "SPAD BUFFERS\n")
               (put-text-property heading-startpoint (point) 'face 'axiom-buffer-menu-group-heading)
               (setq axiom-buffer-menu-startpoint-spad (point)))
              ((eql show-type :help)
               (princ "HELP BUFFERS\n")
               (put-text-property heading-startpoint (point) 'face 'axiom-buffer-menu-group-heading)
               (setq axiom-buffer-menu-startpoint-help (point)))))
      (dolist (buffer (buffer-list))
        (let (this-buffer-read-only
              this-buffer-hidden
              this-buffer-name
              this-buffer-mode
              this-buffer-filename
              name-startpoint
              name-endpoint)
          (with-current-buffer buffer
            (setq this-buffer-read-only buffer-read-only
                  this-buffer-name (buffer-name)
                  this-buffer-mode major-mode
                  this-buffer-filename (buffer-file-name))
            (setq this-buffer-hidden (eql (elt this-buffer-name 0) ?\s)))
          (when (and (or (and (eql show-type :input) (eql this-buffer-mode 'axiom-input-mode))
                         (and (eql show-type :spad) (eql this-buffer-mode 'axiom-spad-mode))
                         (and (eql show-type :help) (eql this-buffer-mode 'axiom-help-mode)))
                     (not this-buffer-hidden))
            (indent-to axiom-buffer-menu-startcolumn-bufprop)
            (princ (if (buffer-modified-p buffer) "*" " "))
            (princ (if this-buffer-read-only "%" " "))
            (indent-to axiom-buffer-menu-startcolumn-bufname)
            (setq name-startpoint (point))
            (princ (axiom-buffer-menu-make-truncated-name
                    this-buffer-name
                    (if (or (eql show-type :input) (eql show-type :spad))
                        (1- (- axiom-buffer-menu-startcolumn-bufpath
                               axiom-buffer-menu-startcolumn-bufname))
                      nil)))
            (setq name-endpoint (point))
            ;; Put the buffer name into a text property
            ;; so we don't have to extract it from the text.
            ;; This way we avoid problems with unusual buffer names.
            (put-text-property name-startpoint name-endpoint 'buffer-name this-buffer-name)
            (put-text-property name-startpoint name-endpoint 'mouse-face 'highlight)
            (put-text-property name-startpoint name-endpoint 'help-echo this-buffer-name)
            (indent-to axiom-buffer-menu-startcolumn-bufpath 1)
            (when this-buffer-filename
              (princ (abbreviate-file-name this-buffer-filename)))
            (princ "\n"))))
      (princ "\n"))
    (axiom-buffer-menu-mode)
    (goto-char axiom-buffer-menu-startpoint-cursor)))

;;;###autoload
(define-derived-mode axiom-buffer-menu-mode special-mode "Axiom Buffer Menu"
  "Major mode for giving users quick-access to Axiom buffers.
\\<axiom-buffer-menu-mode-map>
\\[axiom-buffer-menu-cycle-groups-forward] -- move to next buffer group (file, directory or scratch).
\\[axiom-buffer-menu-cycle-groups-backward] -- move to previous buffer group (file, directory or scratch).
\\[axiom-buffer-menu-cycle-forward] -- move to next active line.
\\[axiom-buffer-menu-cycle-backward] -- move to previous active line.
\\[axiom-buffer-menu-select] -- select the buffer named on the current line.
\\[axiom-buffer-menu-mouse-select] -- select the buffer clicked on with mouse-1.
\\[axiom-buffer-menu-select-other-window] -- display the buffer named on the current line in another window.
\\[axiom-buffer-menu-kill-buffer] -- kill the buffer named on the current line.
\\[axiom-buffer-menu-bury-buffer] -- bury the buffer named on the current line (move to bottom of list).
\\[axiom-buffer-menu-quit] -- kill the menu buffer, return to previous buffer.
\\[describe-mode] -- describe mode."
  :group 'axiom
  (setq truncate-lines t))

;;;###autoload
(defun axiom-buffer-menu ()
  "Display a list of Axiom buffers."
  (interactive)
  (setq axiom-buffer-menu-invoking-buffer (current-buffer))
  (axiom-buffer-menu-prepare-buffer)
  (let ((popup (display-buffer axiom-buffer-menu-bufname '(display-buffer-same-window) t)))
    (when (and popup axiom-select-popup-windows)
      (select-window popup))))

(provide 'axiom-buffer-menu)

;;; axiom-buffer-menu.el ends here
