;;; googledocstrings.el --- Google style docstring insertion -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Maintainer: Doug Davis <ddavis@ddavis.io>
;; URL: https://github.com/brendan-m-murphy/googledocstrings.el
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (s "1.12.0") (dash "2.18.0"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a function to automatically generate NumPy
;; style docstrings for Python functions: `numpydoc-generate'. The
;; NumPy docstring style guide can be found at
;; https://numpydoc.readthedocs.io/en/latest/format.html
;;
;; There are three ways that one can be guided to insert descriptions
;; for the components:
;;
;; 1. Minibuffer prompt (the default).
;; 2. yasnippet expansion (requires `yasnippet' to be installed)
;; 3. Nothing (placeholding template text is inserted).
;;
;; Convenience functions are provided to interactively configure the
;; insertion style symbol:
;; - `numpydoc-use-prompt'
;; - `numpydoc-use-yasnippet'
;; - `numpydoc-use-templates'
;;
;;; Code:

(require 'cl-lib)
(require 'python)
(require 'subr-x)

(require 'dash)
(require 's)

;; forward declare some yasnippet code.
(defvar yas-indent-line)
(declare-function yas-expand-snippet "yasnippet")

;;; customization code.

(defgroup googledocstrings nil
  "NumPy docstrings."
  :group 'convenience
  :prefix "googledocstrings-")

(defcustom googledocstrings-insertion-style 'prompt
  "Which insertion guide to use when generating the docstring.
When set to 'prompt the minibuffer will be used to prompt for
docstring components. Setting to 'yas requires yasnippet to be
installed and `yas-expand-snippet' will be used to insert components.
When nil, template text will be inserted."
  :group 'googledocstrings
  :type '(choice (const :tag "None" nil)
                 (const :tag "Prompt" prompt)
                 (const :tag "Yasnippet" yas)))

(defcustom googledocstrings-quote-char ?\"
  "Character for docstring quoting style (double or single quote)."
  :group 'googledocstrings
  :type 'character)

(defcustom googledocstrings-insert-examples-block nil
  "Flag to control if Examples section is inserted into the buffer."
  :group 'googledocstrings
  :type 'boolean)

(defcustom googledocstrings-insert-parameter-types nil
  "Flag to control if Parameter types are inserted based on type hints."
  :group 'googledocstrings
  :type 'boolean)

(defcustom googledocstrings-insert-raises-block t
  "Flag to control if the Raises section is inserted.
This section will only be inserted if the flag is on and the function
body has raise statements."
  :group 'googledocstrings
  :type 'boolean)

(defcustom googledocstrings-insert-return-without-typehint nil
  "Flag to control inserting a Return block if a type hint is absent."
  :group 'googledocstrings
  :type 'boolean)

(defcustom googledocstrings-template-short "FIXME: Short description."
  "Template text for the short description in a docstring."
  :group 'googledocstrings
  :type 'string)

(defcustom googledocstrings-template-long "FIXME: Long description."
  "Template text for the long description in a docstring."
  :group 'googledocstrings
  :type 'string)

(define-obsolete-variable-alias
  'googledocstrings-template-desc 'googledocstrings-template-arg-desc
  "googledocstrings 0.4")

(defcustom googledocstrings-template-arg-desc "FIXME: Add docs."
  "Template text for individual component descriptions.
This will be added for individual argument and return description
text, and below the Examples section."
  :group 'googledocstrings
  :type 'string)

(defcustom googledocstrings-template-type-desc "FIXME: Add type."
  "Template text for individual component type descriptions."
  :group 'googledocstrings
  :type 'string)

(defcustom googledocstrings-ignored-params (list "" "self" "cls" "*" "*args" "**kwargs" "/")
  "All function parameters with names listed here will be ignored
when generating a docstring."
  :group 'googledocstrings
  :type '(repeat string))

(defcustom googledocstrings-auto-fill-paragraphs t
  "Flag to control automatic paragraph filling.
If set to t text that is inserted in a prompt will be automatically
paragraph-filled."
  :group 'googledocstrings
  :type 'boolean)

;;; package implementation code.

(cl-defstruct googledocstrings--def
  args
  rtype
  raises)

(cl-defstruct googledocstrings--arg
  name
  type
  defval)

(defconst googledocstrings--yas-replace-pat "--NPDOCYAS--"
  "Temporary text to be replaced for yasnippet usage.")

(defun googledocstrings--prompt-p ()
  (eq googledocstrings-insertion-style 'prompt))

(defun googledocstrings--yas-p ()
  (eq googledocstrings-insertion-style 'yas))

(defun googledocstrings--none-to-optional (type)
  (replace-regexp-in-string (rx " | None" eos) ", optional" type t t))

(defun googledocstrings--arg-str-to-struct (argstr)
  "Convert ARGSTR to an instance of `googledocstrings--arg'.
The argument takes on one of four possible styles:
1. First we check for a typed argument with a default value, so it
   contains both ':' and '='. Example would be 'x: int = 5'.
2. Then we check for a typed argument without a default value,
   containing only ':'. Example would be 'x: int'.
3. Then we check for an untyped argument with a default value,
   containing only '='. Example would be 'x=5'.
4. Finally the default is an untyped argument without a default
   value. Example would be `x`."
  (cond (;; type hint and default value (or maybe a dict without a typehint)
         (and (s-contains-p ":" argstr) (s-contains-p "=" argstr))
         (let* ((comps1 (s-split-up-to "=" argstr 1))
                (comps2 (s-split-up-to ":" (car comps1) 1))
                (defval (s-trim (cadr comps1)))
                (name (s-trim (car comps2)))
                (type (cadr comps2)))
           (make-googledocstrings--arg :name name
                               :type (if type
                                         (googledocstrings--none-to-optional (s-trim type))
                                       nil)
                               :defval defval)))
        ;; only a typehint
        ((and (string-match-p ":" argstr)
              (not (s-contains-p "=" argstr)))
         (let* ((comps1 (s-split-up-to ":" argstr 1))
                (name (s-trim (car comps1)))
                (type (s-trim (cadr comps1))))
           (make-googledocstrings--arg :name name
                               :type (googledocstrings--none-to-optional type)
                               :defval nil)))
        ;; only a default value
        ((s-contains-p "=" argstr)
         (let* ((comps1 (s-split-up-to "=" argstr 1))
                (name (s-trim (car comps1)))
                (defval (s-trim (cadr comps1))))
           (make-googledocstrings--arg :name name
                               :type nil
                               :defval defval)))
        ;; only a name
        (t (make-googledocstrings--arg :name argstr
                               :type nil
                               :defval nil))))

(defun googledocstrings--split-args (fnargs)
  "Split FNARGS on comma but ignore those in type [brackets]."
  (let ((bc 0)
        (indquote nil)
        (insquote nil)
        (cursor -1)
        (strs '()))
    (dotimes (i (length fnargs))
      (let ((ichar (aref fnargs i)))
        (cond ((= ichar ?\[) (setq bc (1+ bc)))
              ((= ichar ?\]) (setq bc (1- bc)))
              ((= ichar ?\() (setq bc (1+ bc)))
              ((= ichar ?\)) (setq bc (1- bc)))
              ((= ichar ?\{) (setq bc (1+ bc)))
              ((= ichar ?\}) (setq bc (1- bc)))
              ((= ichar ?\") (if indquote
                                 (setq bc (1- bc)
                                       indquote nil)
                               (setq bc (1+ bc)
                                     indquote t)))
              ((= ichar ?\') (if insquote
                                 (setq bc (1- bc)
                                       insquote nil)
                               (setq bc (1+ bc)
                                     insquote t)))
              ((and (= ichar ?,) (= bc 0))
               (setq strs (append strs (list (substring fnargs
                                                        (1+ cursor)
                                                        i))))
               (setq cursor i)))))
    (setq strs (append strs (list (substring fnargs (1+ cursor)))))))

(defun googledocstrings--extract-def-sig ()
  "Extract function definition string from the buffer.
This function assumes the cursor to be in the function body."
  (save-excursion
    (buffer-substring-no-properties
     (progn
       (python-nav-beginning-of-defun)
       (point))
     (progn
       (python-nav-end-of-statement)
       (point)))))

(defun googledocstrings--parse-def (buffer-substr)
  "Parse the BUFFER-SUBSTR; return instance of googledocstrings--def."
  (save-excursion
    (condition-case nil
        (progn
          (let* ((fnsig buffer-substr)
                 ;; trimmed string of the function signature
                 (trimmed (s-collapse-whitespace fnsig))
                 ;; split into parts (args and return type)
                 (parts (s-split "->" trimmed))
                 ;; raw return
                 (rawret (if (nth 1 parts)
                             (s-trim (nth 1 parts))
                           nil))
                 ;; save return type as a string (or nil)
                 (rtype (when rawret
                          (substring rawret 0 (1- (length rawret)))))
                 ;; raw signature without return type as a string
                 (rawsig (cond (rtype (substring (s-trim (car parts)) 0 -1))
                               (t (substring (s-trim (car parts)) 0 -2))))
                 ;; function args as strings
                 (rawargs (-map #'s-trim
                                (googledocstrings--split-args
                                 (substring rawsig
                                            (1+ (string-match-p (regexp-quote "(")
                                                                rawsig))))))
                 ;; function args as a list of structures (remove some special cases)
                 (args (-remove (lambda (x)
                                  (-contains-p googledocstrings-ignored-params
                                               (googledocstrings--arg-name x)))
                                (-map #'googledocstrings--arg-str-to-struct rawargs)))
                 ;; look for exceptions in the function body
                 (exceptions (googledocstrings--find-exceptions)))
            (make-googledocstrings--def :args args :rtype rtype :raises exceptions)))
      (error "Failed to parse function signature (bad Python syntax)."))))

(defun googledocstrings--has-existing-docstring-p ()
  "Check for an existing docstring.
This function assumes the cursor to be in the function body."
  (save-excursion
    (python-nav-beginning-of-defun)
    (python-nav-end-of-statement)
    (end-of-line)
    (right-char)
    (back-to-indentation)
    (right-char 1)
    (and (eq googledocstrings-quote-char (preceding-char))
         (eq googledocstrings-quote-char (following-char))
         (progn
           (right-char)
           (eq googledocstrings-quote-char (preceding-char)))
         t)))

(defun googledocstrings--detect-indent ()
  "Detect necessary indent for current function docstring.
This function assumes the cursor to be in the function body."
  (save-excursion
    (let ((beg (progn
                 (python-nav-beginning-of-defun)
                 (point)))
          (ind (progn
                 (back-to-indentation)
                 (point))))
      (+ python-indent-offset (- ind beg)))))

(defun googledocstrings--fnsig-range ()
  "Find the beginning and end of the function signature.
This function assumes the cursor to be in the function body."
  (save-excursion
    (vector (progn (python-nav-beginning-of-defun) (point))
            (progn (python-nav-end-of-statement) (point)))))

(defun googledocstrings--function-range ()
  "Find the beginning and end of the function definition.
This function assumes the cursor to be in the function body."
  (save-excursion
    (vector (progn (python-nav-beginning-of-defun) (point))
            (progn (python-nav-end-of-defun) (point)))))

(defun googledocstrings--find-exceptions ()
  "Find exceptions in the function body.
This function assumes the cursor to be in the function body."
  (save-excursion
    (let ((lines '())
          (fnrange (googledocstrings--function-range))
          (pat (rx (one-or-more blank)
                   "raise"
                   (= 1 blank)
                   (any upper-case)
                   anything)))
      (goto-char (elt fnrange 0))
      (while (re-search-forward pat (elt fnrange 1) t)
        (save-excursion
          (let ((p1 (progn
                      (move-beginning-of-line nil)
                      (back-to-indentation)
                      (point)))
                (p2 (progn
                      (move-end-of-line nil)
                      (point))))
            (push (buffer-substring-no-properties p1 p2) lines))))
      (-uniq
       (-map (lambda (x)
               (car (s-split (rx (or eol "("))
                             (s-chop-prefix "raise " x))))
             lines)))))

(defun googledocstrings--lines-in-paragraph ()
  "Count number of lines in current paragraph."
  (save-excursion
    (backward-paragraph)
    (set-mark-command nil)
    (forward-paragraph)
    (- (count-lines (region-beginning) (region-end)) 1)))

(defun googledocstrings--fill-last-insertion ()
  "Fill paragraph on last inserted text."
  (save-excursion
    (move-beginning-of-line nil)
    (back-to-indentation)
    (set-mark-command nil)
    (move-end-of-line nil)
    (fill-paragraph nil t)
    (deactivate-mark))
  ;; if description continues onto multiple lines, indent extra lines 2 spaces
  (if (> (googledocstrings--lines-in-paragraph) 1)
      (save-excursion
        (backward-paragraph)
        (forward-line 2)
        (move-beginning-of-line nil)
        (set-mark-command nil)
        (forward-paragraph)
        (string-insert-rectangle (region-beginning) (region-end) "  ")
        (deactivate-mark))
    nil))

(defun googledocstrings--insert (indent &rest lines)
  "Insert all elements of LINES at indent level INDENT."
  (dolist (s lines)
    (insert (format "%s%s" (make-string indent ?\s) s))))

(defun googledocstrings--insert-short-and-long-desc (indent)
  "Insert short description with INDENT level."
  (let ((ld nil)
        (tmps (cond ((googledocstrings--yas-p) googledocstrings--yas-replace-pat)
                    (t googledocstrings-template-short)))
        (tmpl (cond ((googledocstrings--yas-p) googledocstrings--yas-replace-pat)
                    (t googledocstrings-template-long))))
    (insert "\n")
    (googledocstrings--insert indent
                      (concat (make-string 3 googledocstrings-quote-char)
                              (if (googledocstrings--prompt-p)
                                  (read-string
                                   (format "Short description: "))
                                tmps)
                              "\n\n")
                      (make-string 3 googledocstrings-quote-char))
    (forward-line -1)
    (beginning-of-line)
    (if (googledocstrings--prompt-p)
        (progn
          (setq ld (read-string (concat "Long description "
                                        "(or press return to skip): ")
                                nil nil "" nil))
          (unless (string-empty-p ld)
            (insert "\n")
            (googledocstrings--insert indent ld)
            (when googledocstrings-auto-fill-paragraphs
              (googledocstrings--fill-last-insertion))
            (insert "\n")))
      (insert "\n")
      (googledocstrings--insert indent tmpl)
      (insert "\n"))))

(defun googledocstrings--insert-item (indent name &optional type)
  "Insert parameter with NAME and TYPE at level INDENT."
  (googledocstrings--insert (+ indent 4)  ;; google doc style has args indented
                    (if type
                        (format "%s (%s): " name type)  ;; google doc style has desc on same line
                      (format "%s: " name))))

(defun googledocstrings--insert-item-and-type (indent name type)
  "Insert parameter with NAME and TYPE at level INDENT."
  (let ((tp type)
        (tmpt (cond ((googledocstrings--yas-p) googledocstrings--yas-replace-pat)
                    (t googledocstrings-template-type-desc))))
    (if googledocstrings-insert-parameter-types
        (progn
          (unless tp
            (setq tp (if (googledocstrings--prompt-p)
                         (read-string (format "Type of %s: "
                                              name))
                       tmpt)))
          (googledocstrings--insert-item indent name tp))
      (googledocstrings--insert-item indent name))))

(defun googledocstrings--insert-item-desc (indent element)
  "Insert ELEMENT parameter description at level INDENT."
  (let* ((tmpd (cond ((googledocstrings--yas-p) googledocstrings--yas-replace-pat)
                     (t googledocstrings-template-arg-desc)))
         (desc (concat ;;(make-string 4 ?\s)
                      (if (googledocstrings--prompt-p)
                          (read-string (format "Description for %s: "
                                               element))
                        tmpd))))
    (googledocstrings--insert 0 desc)  ;; zero indent
    (when googledocstrings-auto-fill-paragraphs
      (googledocstrings--fill-last-insertion))
    (insert "\n")))

(defun googledocstrings--insert-parameters (indent fnargs)
  "Insert FNARGS (function arguments) at INDENT level."
  (when fnargs
    (insert "\n")
    (googledocstrings--insert indent "Args:\n")
                      ;; "Parameters\n"
                      ;; "----------\n")
    (dolist (element fnargs)
      (googledocstrings--insert-item-and-type indent
                                      (googledocstrings--arg-name element)
                                      (googledocstrings--arg-type element))
      (googledocstrings--insert-item-desc indent
                                  (googledocstrings--arg-name element)))))

(defun googledocstrings--insert-return (indent fnret)
  "Insert FNRET (return) description (if exists) at INDENT level."
  (let ((tmpr (cond ((googledocstrings--yas-p) googledocstrings--yas-replace-pat)
                    (t googledocstrings-template-arg-desc))))
    (when (or googledocstrings-insert-return-without-typehint
              (and fnret (not (string= fnret "None"))))
      (insert "\n")
      (googledocstrings--insert indent
                        "Returns:\n"
                        ;; "-------\n"
                        (cond (fnret fnret)
                              ((googledocstrings--prompt-p) (read-string "Return type: "))
                              ((googledocstrings--yas-p) googledocstrings--yas-replace-pat)
                              (t googledocstrings-template-type-desc)))
      (insert "\n")
      (googledocstrings--insert indent
                        (concat (make-string 4 ?\s)
                                (if (googledocstrings--prompt-p)
                                    (read-string "Description for return: ")
                                  tmpr)))
      (when googledocstrings-auto-fill-paragraphs
        (googledocstrings--fill-last-insertion))
      (insert "\n"))))

(defun googledocstrings--insert-exceptions (indent fnexcepts)
  "Insert FNEXCEPTS (exception) elements at INDENT level."
  (when (and googledocstrings-insert-raises-block fnexcepts)
    (insert "\n")
    (googledocstrings--insert indent
                      "Raises:\n"
                      ;; "------\n"
                      )
    (dolist (exstr fnexcepts)
      (googledocstrings--insert-item indent exstr)
      (googledocstrings--insert-item-desc indent exstr))))

(defun googledocstrings--insert-examples (indent)
  "Insert function examples block at INDENT level."
  (let ((tmpd (cond ((googledocstrings--yas-p) googledocstrings--yas-replace-pat)
                    (t googledocstrings-template-arg-desc))))
    (when googledocstrings-insert-examples-block
      (insert "\n")
      (googledocstrings--insert indent
                        "Examples:\n"
                        ;; "--------\n"
                        (concat tmpd "\n")))))

(defun googledocstrings--yasnippetfy ()
  "Take the template and convert to yasnippet then execute."
  ;; replace the template
  (save-excursion
    (python-nav-beginning-of-defun)
    (let ((i 1)
          (start (point)))
      (goto-char start)
      (while (re-search-forward googledocstrings--yas-replace-pat nil t)
        (replace-match (format "${%s}" i))
        (setq i (+ 1 i)))))
  ;; execute the yasnippet
  (save-excursion
    (let ((ds-start (progn
                      (python-nav-beginning-of-statement)
                      (forward-char 3)
                      (point)))
          (ds-end (progn
                    (python-nav-end-of-statement)
                    (forward-char -3)
                    (point))))
      (goto-char ds-start)
      (set-mark-command nil)
      (goto-char ds-end)
      (kill-region 1 1 t)
      (yas-expand-snippet (current-kill 0 t)
                          nil nil '((yas-indent-line 'nothing))))))

(defun googledocstrings--insert-docstring (indent fndef)
  "Insert FNDEF with indentation level INDENT."
  (googledocstrings--insert-short-and-long-desc indent)
  (googledocstrings--insert-parameters indent (googledocstrings--def-args fndef))
  (googledocstrings--insert-return indent (googledocstrings--def-rtype fndef))
  (googledocstrings--insert-exceptions indent (googledocstrings--def-raises fndef))
  (googledocstrings--insert-examples indent)
  (when (googledocstrings--yas-p)
    (googledocstrings--yasnippetfy)))

(defun googledocstrings--delete-existing ()
  "Delete existing docstring."
  (let ((3q (make-string 3 googledocstrings-quote-char)))
    (when (googledocstrings--has-existing-docstring-p)
      (python-nav-beginning-of-defun)
      (python-nav-end-of-statement)
      (re-search-forward 3q)
      (left-char 3)
      (set-mark-command nil)
      (re-search-forward 3q)
      (re-search-forward 3q)
      (right-char 1)
      (delete-region (region-beginning) (region-end))
      (deactivate-mark)
      (indent-for-tab-command))))

;;; public API

;;;###autoload
(defun googledocstrings-use-yasnippet ()
  "Enable yasnippet insertion (see `googledocstrings-insertion-style')."
  (interactive)
  (setq googledocstrings-insertion-style 'yas))

;;;###autoload
(defun googledocstrings-use-prompt ()
  "Enable minibuffer prompt insertion (see `googledocstrings-insertion-style')."
  (interactive)
  (setq googledocstrings-insertion-style 'prompt))

;;;###autoload
(defun googledocstrings-use-templates ()
  "Enable template text insertion (see `googledocstrings-insertion-style')."
  (interactive)
  (setq googledocstrings-insertion-style nil))

;;;###autoload
(defun googledocstrings-generate ()
  "Generate NumPy style docstring for Python function.
Assumes that the current location of the cursor is somewhere in the
function that is being documented."
  (interactive)
  (let ((good-to-go t)
        (fnsig (googledocstrings--extract-def-sig)))
    (when (googledocstrings--has-existing-docstring-p)
      (if (y-or-n-p "Docstring exists; destroy and start new? ")
          (googledocstrings--delete-existing)
        (setq good-to-go nil)))
    (when good-to-go
      (python-nav-beginning-of-defun)
      (python-nav-end-of-statement)
      (googledocstrings--insert-docstring (googledocstrings--detect-indent)
                                  (googledocstrings--parse-def fnsig)))))

;; Local Variables:
;; sentence-end-double-space: nil
;; End:
(provide 'googledocstrings)
;;; googledocstrings.el ends here
