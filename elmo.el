;;; elm-mode.el --- A major mode for editing Elm source code  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Theodor Thornhill

;; Author: Theodor Thornhill
;; Package-Requires: ((emacs "26.3") (project "0.3.0")
;; URL: https://github.com/theothornhill/elm
;; Package-Version: 0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple major mode for editing Elm source code

;;; Code:
(require 'project)

(defconst elm--regexp-function-line-beginning
  "^\\([a-z_][0-9A-Za-z_']*\\|([^)]+)\\)"
  "Regexp matching camelCase on line beginning.")

(defconst elm--regexp-type
  "\\b[A-Z][0-9A-Za-z_']*"
  "Regexp matching PascalCase types.")

(defgroup elm nil
  "Support for the Elm Programming Language."
  :link '(url-link :tag "Github" "https://github.com/theothornhill/elmo")
  :group 'languages)

(defconst elm--reserved-keywords
  '("if" "then" "else"
    "case" "of"
    "let" "in"
    "type" "alias"
    "module"
    "where"
    "import" "as" "hiding" "exposing"
    "port" "export" "foreign"
    "perform"
    "deriving")
  "Reserved keywords.")

(defconst elm--starter-syms
  '("import" "module" "type" "port")
  "Keywords starting at indentation 0.")

(defcustom elm-root-file "elm.json"
  "The file signifying root of a project."
  :type 'string
  :group 'elm
  :safe #'stringp)

(defcustom elm-indent-offset 4
  "Indent Elm code by this number of spaces."
  :type 'integer
  :group 'elm
  :safe #'integerp)

(defcustom elm-compile-command "elm make src/Main.elm --output elm.js"
  "Command to use in `project-compile' for Elm projects"
  :type 'string
  :group 'elm
  :safe #'stringp)

(defcustom elm-indent-positions '(plus same)
  "Possible cycling order positions for indentation.

See `recenter-positions'"
  :group 'elm)

(defun elm-beginning-of-defun (&optional arg)
  (interactive "p")
  (unless arg (setq arg 1))
  (re-search-backward elm--regexp-function-line-beginning nil t arg)
  (unless (looking-at (regexp-opt elm--starter-syms))
    (forward-line -1)))

(defun elm-end-of-defun (&optional arg)
  (interactive "p")
  (unless arg (setq arg 1))
  (or (re-search-forward "^\n\n" nil t arg)
      (end-of-buffer)))

(defvar elm-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'project-compile)
    map)
  "Keymap for Elm major mode.")

;;; Indentation

(defmacro elm--find-indentation-of-tokens (tokens)
  `(save-excursion
     (re-search-backward (regexp-opt ',tokens) (point-min) t)
     (current-indentation)))

(defmacro elm--two-lines-same-token-p (token)
  "Checks if line and previous line start with same token."
  `(and (looking-at ,token)
        (save-excursion
          (forward-line -1)
          (back-to-indentation)
          (looking-at ,token))))

(defmacro elm--previous-line-ends-with (tokens)
  `(save-excursion
     (forward-line -1)
     (end-of-line)
     (looking-back (regexp-opt ',tokens))))

(defmacro elm--previous-line-starts-with (tokens)
  `(save-excursion
     (forward-line -1)
     (back-to-indentation)
     (looking-at (regexp-opt ',tokens))))

(defun elm-indent-line ()
  "Set indent levels for Elm source code.  

When indentation is ambiguous, we present three options, MINUS,
SAME and PLUS.  We cycle through them with indentation is pressed
multiple times.  Otherwise, just indent to the correct level."
  (interactive)
  (let* ((indent-level-previous-line
          (save-excursion
            (forward-line -1)
            (current-indentation)))
         (positive-offset (+ indent-level-previous-line elm-indent-offset))
         (negative-offset (- indent-level-previous-line elm-indent-offset)))
    (setq indent-levels
          (if (eq this-command last-command)
              (car (or (cdr (member indent-levels elm-indent-positions))
                       elm-indent-positions))
            (car elm-indent-positions)))
    (let ((indent
           (save-excursion
             (back-to-indentation)
             ;; Now we are positioned at start of indentation.
             ;; Logic below assumes this is true.
             (cond
              ((looking-at (regexp-opt elm--starter-syms)) 0)
              ((looking-at elm--regexp-function-line-beginning) 0)
              ((looking-at (regexp-opt '("{-" "-}"))) 0)
              ((elm--previous-line-ends-with (":" "=")) positive-offset)
              ((and (= indent-level-previous-line 0) (looking-at "=")) positive-offset)
              ((save-excursion (end-of-line) (looking-back "="))
               (+ (elm--find-indentation-of-tokens ("let")) elm-indent-offset))
              ((looking-at "let") positive-offset)
              ((elm--previous-line-starts-with ("in")) indent-level-previous-line)
              ((looking-at "}") (elm--find-indentation-of-tokens ("{")))
              ((looking-at "]") (elm--find-indentation-of-tokens ("[")))
              ((looking-at ")") (elm--find-indentation-of-tokens ("(")))
              ((looking-at "|") (elm--find-indentation-of-tokens ("=")))
              ((looking-at "else") (elm--find-indentation-of-tokens ("if" "then")))
              ((looking-at "then") (elm--find-indentation-of-tokens ("if")))
              ((looking-at ",") (elm--find-indentation-of-tokens ("{" "[" "(")))
              ((looking-at "--") indent-level-previous-line)
              ((looking-at "->") indent-level-previous-line)
              ((save-excursion (end-of-line) (looking-back "->"))
               (+ (elm--find-indentation-of-tokens ("case")) elm-indent-offset))
              ((elm--two-lines-same-token-p "|>") indent-level-previous-line)
              ((elm--two-lines-same-token-p "(") indent-level-previous-line)
              ((looking-at "in") (elm--find-indentation-of-tokens ("let")))
              ((elm--previous-line-ends-with ("=" "->" "<-" "(" "[" "{" "of" "if" "else" "then"))
               positive-offset)
              ;; Cycling of offsets
              ((eq indent-levels 'minus) negative-offset)
              ((eq indent-levels 'same) indent-level-previous-line)
              ((eq indent-levels 'plus) positive-offset)))))
      (if (<= (current-column) (current-indentation))
          (ignore-errors (indent-line-to indent))
        (save-excursion (ignore-errors (indent-line-to indent)))))))

;;; Font locking
(defgroup elm-font-lock nil
  "Font locking for Elm code."
  :group 'faces)

(defconst elm--font-lock-keywords
  (append
   `(
     ;; Reserved keywords
     (,(regexp-opt elm--reserved-keywords 'symbols) . font-lock-keyword-face)

     ;; Function names
     (,elm--regexp-function-line-beginning . font-lock-function-name-face)

     ;; Types
     (,elm--regexp-type . font-lock-type-face)
     )))

(defvar elm--syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Operators
    (dolist (op '(?= ?+ ?- ?* ?/))
      (modify-syntax-entry op "." syntax-table))
    
    ;; Symbol constituents
    (modify-syntax-entry ?. "_" syntax-table)
    
    ;; Block Comments
    (modify-syntax-entry ?\{  "(}1nb" syntax-table)
    (modify-syntax-entry ?\}  "){4nb" syntax-table)
    (modify-syntax-entry ?-  ". 123" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" syntax-table)
    (modify-syntax-entry ?\\ "\\" syntax-table)
    syntax-table))

;;;###autoload
(define-derived-mode elm-mode prog-mode "Elm"
  "Major mode for Elm code."
  :group 'elm
  :syntax-table elm--syntax-table
  
  ;; Movement
  (setq-local beginning-of-defun-function #'elm-beginning-of-defun)
  (setq-local end-of-defun-function #'elm-end-of-defun)
  
  ;; Indentation
  (setq-local indent-line-function 'elm-indent-line)
  
  ;; TODO: propertization
  
  ;; Fonts
  (setq-local font-lock-multiline t)
  (setq-local font-lock-defaults '(elm--font-lock-keywords
                                   nil nil nil nil))
  
  ;; Comments
  (setq-local comment-start-skip "-- ")
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local paragraph-start (concat " *{-\\| *-- |\\|" page-delimiter))
  (setq-local paragraph-separate (concat " *$\\| *\\({-\\|-}\\) *$\\|" page-delimiter))
  
  ;; Misc
  (setq-local open-paren-in-column-0-is-defun-start nil)

  ;; Compilation
  (add-hook 'elm-mode-hook 'elm--set-compile-command))

(defun elm-project-root (dir)
  "Create the cons cell `project-root' needs to discover root."
  (let ((root (locate-dominating-file dir elm-root-file)))
    (when root
      (cons 'elm root))))
  
(cl-defmethod project-root ((project (head elm)))
  (cdr project))

(cl-defmethod project-ignores ((project (head elm)) dir)
  (append vc-directory-exclusion-list
          (list "./elm-stuff/")))

(add-hook 'project-find-functions #'elm-project-root)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))

(provide 'elm-mode)
;;; elm.el ends here
 
