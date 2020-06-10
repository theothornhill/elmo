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
(require 'smie)

(defconst elm--regexp-function-line-beginning
  "^\\([a-z_][0-9A-Za-z_']*\\|([^)]+)\\)"
  "Regexp matching camelCase on line beginning.")

(defconst elm--regexp-function-type-annotation
  (concat elm--regexp-function-line-beginning "\s:\s")
  "Regexp matching a type annotation starting on line beginning.")

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
    "effect" "module"
    "where"
    "import" "as" "hiding" "exposing"
    "port" "export" "foreign"
    "perform"
    "deriving")
  "Reserved keywords.")

(defconst elm--starter-syms
  '("import" "module" "type" "port" "effect")
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

(defcustom elm-indent-positions '(same plus)
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
    (define-key map (kbd "C-c C-z") 'elm-repl)
    (define-key map (kbd "C-c C-r") 'elm-reactor)
    map)
  "Keymap for Elm major mode.")

;;; Tooling integration

(defmacro elm--with-project-root (&rest body)
  (declare (debug t) (indent 0))
  `(let ((default-directory (project-root (project-current))))
     ,@body))

(defun elm-repl ()
  "Create a new buffer with Elm repl started and switch to it."
  (interactive)
  (elm--with-project-root
    (switch-to-buffer-other-window
     (make-comint "Elm Repl" "elm" nil "repl"))))

(defun elm-reactor ()
  "Create a new buffer with Elm reactor started and open it."
  (interactive)
  (elm--with-project-root
    (display-buffer
     (make-comint "Elm Reactor" "elm" nil "reactor"))))

;;; Indentation - SMIE

(defun elm-smie-forward-token ()
  (cond ;; ((looking-at "\n\n\n")
        ;;  (progn (forward-char 3) ";"))
   ((looking-at elm--regexp-function-line-beginning)
    (progn (forward-word 1) "fun"))
        (t
         (forward-comment (point-max))
         (buffer-substring-no-properties
          (point)
          (progn
            (if (zerop (skip-syntax-forward "w_'"))
                (skip-syntax-forward "."))
            (point))))))

(defun elm-smie-backward-token ()
  (cond ;; ((looking-back "\n\n\n")
        ;;  (progn (forward-char -3) ";"))
        ((looking-back elm--regexp-function-line-beginning)
         (progn (forward-word -1) "fun"))
        (t
         (forward-comment (- (point)))
         (buffer-substring-no-properties
          (point)
          (progn
            (if (zerop (skip-syntax-backward "."))
                (skip-syntax-backward "w_'"))
            (point))))))

(defconst elm-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (expr ("type" id "=" sexp)
             ("if" expr "then" expr "else" expr)
             ("let" decls "in" expr)
             ("case" expr "of" branches))
       (sexp (id ":" type)
             ("(" exprs ")")
             ("{" exprs "}"))
       (exprs (exprs "," exprs)
              (expr))
       (type (type "->" type))
       (branches (branches "|" branches))
       (decls (id "=" expr)
              ("fun" "=" sexp)
              (decls "import" decls)
              (decls "module" decls))
       (toplevel (decls)
                 (expr)))
     '((assoc "import" "module"))
     '((assoc "|"))
     '((assoc "->"))
     '((assoc ";;") (assoc "="))
     '((assoc "&&") (assoc "||") (noassoc ":"))
     '((assoc ";"))
     '((assoc ",")))
    (smie-precs->prec2
     '((nonassoc ">" ">=" "<>" "<" "<=" "==")
       (assoc "+" "-" "^")
       (assoc "/" "*"))))))

(defun elm-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) elm-indent-offset)
    (`(:list-intro . "|") 0)
    (`(:before . "fun") 0)
    (`(:after . "in") 0)
    (`(:after . "->") (if (smie-rule-hanging-p) elm-indent-offset))
    (`(:after . "then") elm-indent-offset)
    (`(:after . "else") elm-indent-offset)
    (`(:after . "of") elm-indent-offset)
    (`(:before . "|") 0)
    (`(:before . "=") (if (smie-rule-bolp) elm-indent-offset))
    (`(:after . "=") (if (smie-rule-hanging-p) elm-indent-offset))
    (`(:before . ",") 0)))


(defun elm--set-compile-command ()
  (set (make-local-variable 'compile-command) elm-compile-command))

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
    (mapc (lambda (c) (modify-syntax-entry c "."  syntax-table)) "<%>&$+=-/:><?@`^|")
    ;; Symbol constituents
    (modify-syntax-entry ?. "_" syntax-table)
    ;; Block Comments
    ;; (modify-syntax-entry ?\{  "(}1nb" syntax-table)
    ;; (modify-syntax-entry ?\}  "){4nb" syntax-table)
    (modify-syntax-entry ?\( "()" syntax-table)
    (modify-syntax-entry ?\) ")(" syntax-table)
    (modify-syntax-entry ?\{ "(}" syntax-table)
    (modify-syntax-entry ?\} "){" syntax-table)
    (modify-syntax-entry ?\[ "(]" syntax-table)
    (modify-syntax-entry ?\] ")[" syntax-table)

    ;; Comments
    (modify-syntax-entry ?-  ". 12" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)

    ;; (modify-syntax-entry ?\" "\"" syntax-table)
    (modify-syntax-entry ?\\ "\\" syntax-table)
    syntax-table)
  "The syntax table used in `elm-mode'")

;;;###autoload
(define-derived-mode elm-mode prog-mode "Elm"
  "Major mode for Elm code."
  :group 'elm
  :syntax-table elm--syntax-table
  
  ;; Movement
  (setq-local beginning-of-defun-function #'elm-beginning-of-defun)
  (setq-local end-of-defun-function #'elm-end-of-defun)
  
  ;; Indentation
  ;; (setq-local indent-line-function 'elm-indent-line)
  
  (when (boundp 'electric-indent-inhibit) (setq electric-indent-inhibit t))
  (smie-setup elm-smie-grammar #'elm-smie-rules
              :forward-token #'elm-smie-forward-token
              :backward-token #'elm-smie-backward-token)
  
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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))

(provide 'elm-mode)
;;; elm.el ends here
 
