;;; consult-denote.el --- Use Consult in tandem with Denote -*- lexical-binding: t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/consult-denote
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1") (denote "2.3.0") (consult "1.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; WORK-IN-PROGRESS.

;;; Code:

(require 'consult)
(require 'denote)

(defgroup consult-denote ()
  "Simple notes with an efficient file-naming scheme."
  :group 'files
  :group 'minibuffer
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

;;;; User options

(defcustom consult-denote-grep-command #'consult-grep
  "Consult-powered Grep command to use for `consult-denote-grep'."
  :type 'function)

(defcustom consult-denote-find-command #'consult-find
  "Consult-powered Find command to use for `consult-denote-find'."
  :type 'function)

;;;; Functions

(defun consult-denote-file-prompt (&optional files-matching-regexp prompt-text)
  "A Consult-powered equivalent of `denote-file-prompt'.
The FILES-MATCHING-REGEXP and PROMPT-TEXT have the same meaning as the
aforementioned function."
  (when-let ((all-files (denote-directory-files files-matching-regexp :omit-current)))
    (let* ((common-parent-directory
            (let ((common-prefix (try-completion "" all-files)))
              (if (> (length common-prefix) 0)
                  (file-name-directory common-prefix))))
           (cpd-length (length common-parent-directory))
           (prompt-prefix (or prompt-text "Select FILE"))
           (prompt (if (zerop cpd-length)
                       (format "%s: " prompt-prefix)
                     (format "%s in %s: " prompt-prefix common-parent-directory)))
           (included-cpd (when (member common-parent-directory all-files)
                           (setq all-files
                                 (delete common-parent-directory all-files))
                           t))
           (substrings (mapcar (lambda (s) (substring s cpd-length)) all-files))
           (_ (when included-cpd
                (setq substrings (cons "./" substrings))))
           (new-collection (denote--completion-table 'file substrings))
           (relname
            (let ((default-directory (denote-directory)))
              (consult--read new-collection
                             :state (consult--file-preview)
                             :prompt prompt
                             :history 'denote-file-history)))
           (absname (expand-file-name relname common-parent-directory)))
      ;; NOTE 2024-02-29: This delete and add feels awkward.  I wish
      ;; we could tell `completing-read' to just leave this up to us.
      (setq denote-file-history (delete relname denote-file-history))
      (add-to-history 'denote-file-history absname)
      absname)))

(defun consult-denote-select-file-prompt (files)
  "Prompt for Denote file among FILES."
  (let* ((default-directory denote-directory)
         (file-names (mapcar #'denote-get-file-name-relative-to-denote-directory files)))
    (consult--read
     (denote--completion-table 'file file-names)
     :prompt "Select FILE: "
     :require-match t
     :state (consult--file-preview)
     :history 'denote-link-find-file-history)))

;;;; Commands

;;;###autoload
(defun consult-denote-grep ()
  "Call `consult-denote-grep-command' in the variable `denote-directory'."
  (interactive)
  (let ((default-directory denote-directory))
    (funcall-interactively consult-denote-grep-command)))

;;;###autoload
(defun consult-denote-find ()
  "Call `consult-denote-find-command' in the variable `denote-directory'."
  (interactive)
  (let ((default-directory denote-directory))
    (funcall-interactively consult-denote-find-command)))

;;;; Integrate with denote.el

(defvar consult-denote-buffer-history nil)

(defface consult-denote-buffer
  '((t :inherit font-lock-string-face))
  "Face for Denote buffers used `consult-buffer'.")

(defun consult-denote--buffers ()
  "Return file names of Denote buffers."
  (delq nil
        (mapcar
         (lambda (buffer)
           (when-let ((file (buffer-file-name buffer))
                      ((buffer-live-p buffer))
                      ((denote-filename-is-note-p file)))
             (buffer-name buffer)))
         (buffer-list))))

(defvar consult-denote--buffer-source
    `( :name "Denote buffers"
       :narrow ?D
       :category buffer
       :default t
       :face consult-denote-buffer
       :history consult-denote-buffer-history
       :action ,#'switch-to-buffer
       :state ,#'consult--buffer-state
       :items ,#'consult-denote--buffers)
  "Source for `consult-buffer' to list Denote buffers.")

;;;###autoload
(define-minor-mode consult-denote-mode
  "Use Consult in tandem with Denote."
  :global t
  (if consult-denote-mode
      ;; We will eventually have a denote-file-prompt-function and
      ;; `funcall' it, but this is okay for now.  Same for all prompts
      (progn
        (add-to-list 'consult-buffer-sources 'consult-denote--buffer-source)
        (advice-add #'denote-file-prompt :override #'consult-denote-file-prompt)
        (advice-add #'denote-link--find-file-prompt :override #'consult-denote-select-file-prompt))
    ;; TODO 2024-03-27: Remove `'consult-denote--buffer-source' from `'consult-buffer-sources'.
    (advice-remove #'denote-file-prompt #'consult-denote-file-prompt)
    (advice-remove #'denote-link--find-file-prompt #'consult-denote-select-file-prompt)))

(provide 'consult-denote)
;;; consult-denote.el ends here
