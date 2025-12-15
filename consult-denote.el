;;; consult-denote.el --- Use Consult in tandem with Denote -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/consult-denote
;; Version: 0.4.2
;; Package-Requires: ((emacs "28.1") (denote "4.0.0") (consult "2.2"))

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
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(info-link "(consult-denote) Top")
  :link '(url-link :tag "Denote homepage" "https://protesilaos.com/emacs/denote")
  :link '(url-link :tag "Consult Denote homepage" "https://protesilaos.com/emacs/consult-denote"))

;;;; User options

(defcustom consult-denote-grep-command #'consult-grep
  "Consult-powered Grep command to use for `consult-denote-grep'."
  :type 'function
  :package-version '(consult-denote . "0.1.0"))

(defcustom consult-denote-find-command #'consult-find
  "Consult-powered Find command to use for `consult-denote-find'."
  :type 'function
  :package-version '(consult-denote . "0.1.0"))

(defconst consult-denote-all-buffer-sources
  '(consult-denote-buffer-source
    consult-denote-subdirectory-source
    consult-denote-silo-source)
  "All the Denote sources for `consult-buffer'.")

(defcustom consult-denote-buffer-sources consult-denote-all-buffer-sources
  "Sources to add to the `consult-buffer' interface."
  ;; FIXME 2024-05-20: I must be missing something obvious because any
  ;; symbol I give it is accepted.
  :type '(repeat
          (symbol :match (lambda (_widget value)
                           (memq value consult-denote-all-buffer-sources))
                  :type-error "The value is not among `consult-denote-all-buffer-sources'"))
  :package-version '(consult-denote . "0.1.0"))

;;;; Functions

(defun consult-denote-file-prompt (&optional files-matching-regexp prompt-text no-require-match has-identifier)
  "A Consult-powered equivalent of `denote-file-prompt'.

With optional FILES-MATCHING-REGEXP, filter the candidates per
the given regular expression.

With optional PROMPT-TEXT, use it instead of the default call to
select a file.

With optional NO-REQUIRE-MATCH, accept the given input as-is.

With optional HAS-IDENTIFIER, only show candidates that have an
identifier.

Return the absolute path to the matching file."
  (let* ((roots (denote-directories))
         (single-dir-p (null (cdr roots)))
         (default-directory (if single-dir-p ; setting the `default-directory' is needed for the preview
                                (car roots)
                              (denote-directories-get-common-root roots)))
         (relative-files (mapcar
                          #'denote-get-file-name-relative-to-denote-directory
                          (denote-directory-files
                           (or denote-file-prompt-use-files-matching-regexp files-matching-regexp)
                           :omit-current nil nil has-identifier)))
         (prompt (if single-dir-p
                     (format "%s: " (or prompt-text "Select FILE"))
                   (format "%s in %s: "
                           (or prompt-text "Select FILE")
                           (propertize default-directory 'face 'denote-faces-prompt-current-name))))
         (input (consult--read
                 (apply 'denote-get-completion-table relative-files denote-file-prompt-extra-metadata)
                 :state (consult--file-preview)
                 :require-match (unless no-require-match :require-match)
                 :history 'denote-file-history
                 :prompt prompt))
         (absolute-file (if single-dir-p
                            (expand-file-name input default-directory)
                          input)))
    ;; NOTE: This block is executed when no-require-match is t. It is useful
    ;; for commands such as `denote-open-or-create` or similar.
    (unless (file-exists-p absolute-file)
      (setq denote-file-prompt-latest-input input)
      (setq denote-file-history (delete input denote-file-history)))
    ;; NOTE: We must always return an absolute path, even if it does not
    ;; exist, because callers expect one.  They handle a non-existent file
    ;; appropriately.
    absolute-file))

(declare-function denote-sequence-get-all-files "denote-sequence" (&optional files as-sequence-path-pairs))
(declare-function denote-sequence-get-all-files "denote-sequence" (&optional files as-sequence-path-pairs))

(defun consult-denote-sequence-file-prompt (&optional prompt-text files-with-sequences)
  "A Consult-powered equivalent of `denote-sequence-file-prompt'.

With optional PROMPT-TEXT use it instead of a generic prompt.

With optional FILES-WITH-SEQUENCES as a list of strings, use them as
completion candidates.  Else use `denote-sequence-get-all-files'."
  (if-let* ((files (or files-with-sequences (denote-sequence-get-all-files)))
            (roots (denote-directories))
            (single-dir-p (null (cdr roots)))
            (relative-files (if single-dir-p
                                (mapcar #'denote-get-file-name-relative-to-denote-directory files)
                              files))
            (prompt (format-prompt (or prompt-text "Select FILE with sequence") nil))
            (input (consult--read
                    (apply 'denote-get-completion-table relative-files denote-file-prompt-extra-metadata)
                    :state (consult--file-preview)
                    :require-match nil
                    :history 'denote-sequence-file-history
                    :prompt prompt)))
      (if single-dir-p
          (expand-file-name input (car roots))
        input)
    (error "There are no sequence notes in the `denote-directory'")))

(defun consult-denote-select-linked-file-prompt (files &optional prompt-text)
  "Prompt for linked file among FILES and use optional PROMPT-TEXT."
  (let* ((roots (denote-directories))
         (single-dir-p (null (cdr roots)))
         (relative-files (if single-dir-p
                             (mapcar #'denote-get-file-name-relative-to-denote-directory files)
                           files))
         (prompt (format-prompt (or prompt-text "Find linked file") nil))
         (input (consult--read
                 (apply 'denote-get-completion-table relative-files denote-file-prompt-extra-metadata)
                 :state (consult--file-preview)
                 :require-match t
                 :history 'denote-link-find-file-history
                 :prompt prompt)))
    (if single-dir-p
        (expand-file-name input (car roots))
      input)))

(defvar denote-silo-directory-history)
(defvar denote-silo-directories)

(defun consult-denote-silo-directory-prompt ()
  "Like the `denote-silo-directory-prompt' with Consult preview."
  (let ((default (car denote-silo-directory-history)))
    (consult--read
     (denote-get-completion-table denote-silo-directories '(category . file))
     :state (consult--file-preview)
     :require-match t
     :prompt (format-prompt "Select a silo" default)
     :default default
     :history 'denote-silo-directory-history)))

(declare-function denote-org--get-outline "denote-org" (file))

;; FIXME 2024-07-03: We need a :state function that previews the
;; current line in the given buffer and then restores the window
;; configuration.
(defun consult-denote-outline-prompt (&optional file)
  "Like `denote-org-extras-outline-prompt' with Consult preview.
FILE has the same meaning as in `denote-org-extras-outline-prompt'."
  (let ((current-file (or file buffer-file-name)))
    (consult--read
     (denote-get-completion-table (denote-org--get-outline current-file) '(category . imenu))
     :state (lambda (_action candidate)
              (with-current-buffer (current-buffer)
                (when-let* ((_ candidate)
                            (line (string-to-number (car (split-string candidate)))))
                  (forward-line (- line 1)))))
     :prompt (format "Select heading inside `%s': " (propertize (file-name-nondirectory current-file) 'face 'denote-faces-prompt-current-name))
     :require-match t)))

;;;; Commands

;;;###autoload
(defun consult-denote-grep ()
  "Call `consult-denote-grep-command' in the variable `denote-directory'."
  (declare (interactive-only t))
  (interactive)
  (if (denote-has-single-denote-directory-p)
      (funcall-interactively consult-denote-grep-command (car (denote-directories)))
    (let* ((directories (mapconcat #'identity (denote-directories) " "))
           (consult-grep-args `("grep" (consult--grep-exclude-args) "--null --line-buffered --color=never --ignore-case --with-filename --line-number -I -r" ,directories))
           (consult-ripgrep-args (format "rg --null --line-buffered --color=never --max-columns=1000 --path-separator --smart-case --no-heading --with-filename --line-number --search-zip %s" directories)))
      (funcall-interactively consult-denote-grep-command))))

;;;###autoload
(defun consult-denote-find ()
  "Call `consult-denote-find-command' in the variable `denote-directory'."
  (declare (interactive-only t))
  (interactive)
  (if (denote-has-single-denote-directory-p)
      (funcall-interactively consult-denote-find-command (car (denote-directories)))
    (let* ((directories (mapconcat #'identity (denote-directories) " "))
           (consult-find-args  (format "find %s -not ( -path */.[A-Za-z]* -prune )" directories)))
      (funcall-interactively consult-denote-find-command))))

(consult-customize consult-denote-find :sort t :state (consult--file-preview))

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

(defun consult-denote--buffers ()
  "Return file names of Denote buffers."
  (delq nil
        (mapcar
         (lambda (buffer)
           (when-let* ((file (buffer-file-name buffer))
                       ((buffer-live-p buffer))
                       ((denote-file-has-denoted-filename-p file)))
             (buffer-name buffer)))
         (buffer-list))))

(defvar consult-denote-buffer-source
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

(defvar consult-denote-subdirectory-source
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

(defvar consult-denote-silo-source nil
  "Source for `consult-buffer' to list Denote silos.")

(with-eval-after-load 'denote-silo-extras
  (setq consult-denote-silo-source
        `( :name "Denote silos"
           :narrow ?L
           :category file
           :default t
           :face consult-denote-directory
           :history consult-denote-buffer-history
           :action ,#'dired
           :state ,#'consult--file-state
           :items ,denote-silo-extras-directories)))

(declare-function denote-org-outline-prompt "denote-org" (&optional file))
(declare-function denote-silo-directory-prompt "denote-silo" ())
(declare-function denote-sequence-file-prompt "denote-sequence" (&optional prompt-text files-with-sequences))

;;;###autoload
(define-minor-mode consult-denote-mode
  "Use Consult in tandem with Denote."
  :global t
  (if consult-denote-mode
      ;; We will eventually have a denote-file-prompt-function and
      ;; `funcall' it, but this is okay for now.  Same for all prompts
      (progn
        (dolist (source consult-denote-buffer-sources)
          (add-to-list 'consult-buffer-sources source :append))
        (advice-add #'denote-file-prompt :override #'consult-denote-file-prompt)
        (advice-add #'denote-select-from-files-prompt :override #'consult-denote-select-linked-file-prompt)
        ;; See FIXME where this function is defined.
        (advice-add #'denote-org-outline-prompt :override #'consult-denote-outline-prompt)
        (advice-add #'denote-silo-directory-prompt :override #'consult-denote-silo-directory-prompt)
        (advice-add #'denote-sequence-file-prompt :override #'consult-denote-sequence-file-prompt))
    (dolist (source consult-denote-buffer-sources)
      (setq consult-buffer-sources (delq source consult-buffer-sources)))
    (advice-remove #'denote-file-prompt #'consult-denote-file-prompt)
    (advice-remove #'denote-select-from-files-prompt #'consult-denote-select-linked-file-prompt)
    (advice-remove #'denote-org-outline-prompt #'consult-denote-outline-prompt)
    (advice-remove #'denote-silo-directory-prompt #'consult-denote-silo-directory-prompt)
    (advice-remove #'denote-sequence-file-prompt #'consult-denote-sequence-file-prompt)))

(provide 'consult-denote)
;;; consult-denote.el ends here
