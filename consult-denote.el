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

;; Integrate the `denote' and `consult' packages:
;;
;; - [Denote](https://github.com/protesilaos/denote) : A file-naming
;;   scheme to easily retrieve files of any type.  Useful for note-taking
;;   and long-term storage files.
;; - [Consult](https://github.com/minad/consult): Enhanced interactivity
;;   for the standard Emacs minibuffer, such as a preview mechanism for
;;   buffers and an asynchronous grep/find.
;;
;; The purpose of `consult-denote` is as follows:
;;
;; 1. **Upgrade all the minibuffer prompts of Denote:** For the time
;;    being, this means that we show a preview of the file to-be-linked
;;    or to-be-opened.  Simply enable the `consult-denote-mode'.  The
;;    prompts all use the same patterns as core Denote and *will never
;;    deviate from this paradigm*, such as to prettify titles or whatnot
;;    (that is an expensive operation that slows down Emacs).
;;
;; 2. **Easy search for the `denote-directory`:** Implement
;;    Consult-powered Grep and Find commands which operate on the
;;    `denote-directory` regardless of where they are called from.  See
;;    the commands `consult-denote-grep` and `consult-denote-find'.
;;    Customise which command they call by modifying the user options
;;    `consult-denote-grep-command` and `consult-denote-find-command`.
;;
;; 3. **Include Denote "sources" for `consult-buffer':** This is also
;;    part of the `consult-denote-mode'.  It adds new headings/groups to
;;    the interface of the `consult-buffer` command.  Those lists (i) the
;;    buffers that visit Denote files, (ii) the subdirectories of the
;;    `denote-directory', and (iii) the silos listed in the value of the
;;    user option `denote-silo-extras-directories' (for those who opt in
;;    to that extension).
;;
;; In the future we may use other features of Consult, based on user
;; feedback.

;;; Code:

(require 'consult)
(require 'denote)

(defgroup consult-denote ()
  "Simple notes with an efficient file-naming scheme."
  :group 'files
  :group 'minibuffer
  :link '(url-link :tag "Homepage" " https://github.com/protesilaos/consult-denote"))

;;;; User options

(defcustom consult-denote-grep-command #'consult-grep
  "Consult-powered Grep command to use for `consult-denote-grep'."
  :type 'function
  :package-version '(consult-denote . "0.1.0"))

(defcustom consult-denote-find-command #'consult-find
  "Consult-powered Find command to use for `consult-denote-find'."
  :type 'function
  :package-version '(consult-denote . "0.1.0"))

;;;; Functions

(defun consult-denote-file-prompt (&optional files-matching-regexp prompt-text)
  "A Consult-powered equivalent of `denote-file-prompt'.
The FILES-MATCHING-REGEXP and PROMPT-TEXT have the same meaning as the
aforementioned function."
  (when-let ((all-files (denote-directory-files files-matching-regexp :omit-current)))
    (let* ((default-directory (denote-directory))
           (common-parent-directory
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
           ;; We populate the history ourselves because we process the input.
           (input
            (consult--read new-collection
                           :state (consult--file-preview)
                           :prompt prompt))
           (filename (with-temp-buffer
                       (insert input)
                       (completion-in-region (point-min) (point-max) new-collection)
                       (buffer-string))))
      (setq denote-file-prompt-latest-input input)
      ;; We want to return the user's input verbatim if it does not
      ;; match a file uniquely.
      (if (denote-file-has-identifier-p (expand-file-name filename (denote-directory)))
          (progn
            (setq denote-file-history (delete input denote-file-history))
            (add-to-history 'denote-file-history filename)
            filename)
        input))))

(defun consult-denote-select-linked-file-prompt (files)
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
  (declare (interactive-only t))
  (interactive)
  (funcall-interactively consult-denote-grep-command (denote-directory)))

;;;###autoload
(defun consult-denote-find ()
  "Call `consult-denote-find-command' in the variable `denote-directory'."
  (declare (interactive-only t))
  (interactive)
  (funcall-interactively consult-denote-find-command (denote-directory)))

;;;; Integrate with denote.el

(defvar consult-denote-buffer-history nil)

(defface consult-denote-buffer
  '((t :inherit font-lock-string-face))
  "Face for Denote buffers used `consult-buffer'."
  :package-version '(consult-denote . "0.1.0"))

(defface consult-denote-directory
  '((t :inherit font-lock-constant-face))
  "Face for Denote directories used in `consult-buffer'."
  :package-version '(consult-denote . "0.1.0"))

;; TODO 2024-05-09: Review suggestion by Philip Kaludercic to use `match-buffers'.
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

(defvar consult-denote--subdirectory-source
  `( :name "Denote subdirectories"
     :narrow ?S
     :category file
     :default t
     :face consult-denote-directory
     :history consult-denote-buffer-history
     :action ,#'dired
     :state ,#'consult--file-state
     :items ,#'denote-directory-subdirectories)
  "Source for `consult-buffer' to list Denote subdirectories.")

(defvar consult-denote--silo-source nil
  "Source for `consult-buffer' to list Denote silos.")

(with-eval-after-load 'denote-silo-extras
  (setq consult-denote--silo-source
    `( :name "Denote silos"
       :narrow ?L
       :category file
       :default t
       :face consult-denote-directory
       :history consult-denote-buffer-history
       :action ,#'dired
       :state ,#'consult--file-state
       :items ,denote-silo-extras-directories)))

;; TODO 2024-03-30: Cover the `denote-org-extras--outline-prompt'.  It
;; will be like `consult-outline' in presentation.

;; TODO 2024-03-30: Cover the `denote-silo-extras--directory-prompt'.
;; It is a regular directory prompt.  Preview the dired buffer.

;;;###autoload
(define-minor-mode consult-denote-mode
  "Use Consult in tandem with Denote."
  :global t
  (if consult-denote-mode
      ;; We will eventually have a denote-file-prompt-function and
      ;; `funcall' it, but this is okay for now.  Same for all prompts
      (progn
        (add-to-list 'consult-buffer-sources 'consult-denote--subdirectory-source :append)
        (add-to-list 'consult-buffer-sources 'consult-denote--silo-source :append)
        (add-to-list 'consult-buffer-sources 'consult-denote--buffer-source :append)
        (advice-add #'denote-file-prompt :override #'consult-denote-file-prompt)
        (advice-add #'denote-select-linked-file-prompt :override #'consult-denote-select-linked-file-prompt))
    (setq consult-buffer-sources (delq 'consult-denote--subdirectory-source consult-buffer-sources))
    (setq consult-buffer-sources (delq 'consult-denote--silo-source consult-buffer-sources))
    (setq consult-buffer-sources (delq 'consult-denote--buffer-source consult-buffer-sources))
    (advice-remove #'denote-file-prompt #'consult-denote-file-prompt)
    (advice-remove #'denote-select-linked-file-prompt #'consult-denote-select-linked-file-prompt)))

(provide 'consult-denote)
;;; consult-denote.el ends here
