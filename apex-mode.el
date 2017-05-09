;;; apex-mode.el -- Major mode for Salesforce Apex files

;; Copyright (C) 2017 Magnus Nyberg

;; Author: Magnus Nyberg <magnus@nexter.se>
;; Keywords: languages, apex, force, sfdc, salesforce
;; Homepage: https://github.com/nxtr/apex-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing Apex code.

;;; Code:

(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  (c-add-language 'apex-mode 'java-mode))

;;;; langs

(c-lang-defconst c-identifier-syntax-modifications
  "A list that describes the modifications that should be done to the
mode syntax table to get a syntax table that matches all identifiers
and keywords as words."
  apex '((?@ ."w") (?_ . "w")))

(c-lang-defconst c-symbol-chars
  "Set of characters that can be part of a symbol.
This is of the form that fits inside [ ] in a regexp."
  apex (concat c-alnum "_"))

(c-lang-defconst c-after-id-concat-ops
  "Operators that can occur after a binary operator on `c-identifier-ops'
in identifiers.  nil in languages that don't have such things."
  apex nil)

(c-lang-defconst c-assignment-operators
  "List of all assignment operators."
  apex '("=" "*=" "/=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|=" ">>>="))

(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face."
  apex '("after" "before" "void"))

(c-lang-defconst c-type-prefix-kwds
  "Keywords where the following name - if any - is a type name, and
where the keyword together with the symbol works as a type in
declarations."
  apex '("on"))

(c-lang-defconst c-class-decl-kwds
  "Keywords introducing declarations where the following block (if any)
contains another declaration level that should be considered a class."
  apex '("class" "interface" "trigger"))

(c-lang-defconst c-typeless-decl-kwds
  "Keywords introducing declarations where the \(first) identifier
\(declarator) follows directly after the keyword, without any type."
  apex (append (c-lang-const c-class-decl-kwds)
               (c-lang-const c-brace-list-decl-kwds)
               '("get" "set")))

(c-lang-defconst c-modifier-kwds
  "Keywords that can prefix normal declarations of identifiers
\(and typically act as flags).  Things like argument declarations
inside function headers are also considered declarations in this
sense."
  apex '("abstract" "final" "global" "override" "private" "protected" "public"
         "with" "sharing" "without" "static" "testmethod" "transient"
         "virtual" "webservice"))

(c-lang-defconst c-other-decl-kwds
  "Keywords that can start or prefix any declaration level construct,
besides those on `c-class-decl-kwds', `c-brace-list-decl-kwds',
`c-other-block-decl-kwds', `c-typedef-decl-kwds',
`c-typeless-decl-kwds' and `c-modifier-kwds'."
  apex nil)

(c-lang-defconst c-postfix-decl-spec-kwds
  "Keywords introducing extra declaration specifiers in the region
between the header and the body \(i.e. the \"K&R-region\") in
declarations."
  apex '("extends" "implements"))

(c-lang-defconst c-type-list-kwds
  "Keywords that may be followed by a comma separated list of type
identifiers, where each optionally can be prefixed by keywords.  (Can
also be used for the special case when the list can contain only one
element.)"
  apex '("extends" "implements"))

(c-lang-defconst c-ref-list-kwds
  "Keywords that may be followed by a comma separated list of
reference (i.e. namespace/scope/module) identifiers, where each
optionally can be prefixed by keywords.  (Can also be used for the
special case when the list can contain only one element.)  Assumed to
be mutually exclusive with `c-type-list-kwds'."
  apex nil)

(c-lang-defconst c-block-stmt-1-2-kwds
  "Statement keywords optionally followed by a paren sexp.
Keywords here should also be in `c-block-stmt-1-kwds'."
  apex nil)

(c-lang-defconst c-block-stmt-2-kwds
  "Statement keywords followed by a paren sexp and then by a substatement."
  apex '("for" "if" "while" "catch"))

(c-lang-defconst c-simple-stmt-kwds
  "Statement keywords followed by an expression or nothing."
  apex '("break" "continue" "delete" "insert" "merge" "return" "throw" "undelete"
         "update" "upsert"))

(c-lang-defconst c-case-kwds
  "The keyword(s) which introduce a \"case\" like construct.
This construct is \"<keyword> <expression> :\"."
  apex nil)

(c-lang-defconst c-label-kwds
  "Keywords introducing colon terminated labels in blocks."
  apex nil)

(c-lang-defconst c-before-label-kwds
  "Keywords that might be followed by a label identifier."
  apex nil)

(c-lang-defconst c-inexpr-class-kwds
  "Keywords that can start classes inside expressions."
  apex nil)

(c-lang-defconst c-inexpr-brace-list-kwds
  "Keywords that can start brace list blocks inside expressions.
Note that Java specific rules are currently applied to tell this from
`c-inexpr-class-kwds'."
  apex '("new"))

(c-lang-defconst c-block-prefix-disallowed-chars
  "List of syntactically relevant characters that never can occur before
the open brace in any construct that contains a brace block."
  apex (c--set-difference (c-lang-const c-block-prefix-disallowed-chars)
                          '(?,)))

(c-lang-defconst c-recognize-typeless-decls
  "Non-nil means function declarations without return type should be
recognized."
  apex nil)

(c-lang-defconst c-enums-contain-decls
  "Non-nil means that an enum structure can contain declarations."
  apex nil)

(c-lang-defconst c-recognize-colon-labels
  "Non-nil if generic labels ending with \":\" should be recognized."
  apex nil)

;;;; engine

(defun apex-c-cheap-inside-bracelist-p (paren-state)
  ;; Return the position of the L-brace if point is inside a brace list
  ;; initialization of an array, etc.
  nil)

(advice-add 'c-cheap-inside-bracelist-p :around
            (lambda (cc-fun &rest args)
              (if (eq major-mode 'apex-mode)
                  (apply 'apex-c-cheap-inside-bracelist-p args)
                (apply cc-fun args))))

(defun apex-c-inside-bracelist-p (containing-sexp paren-state)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren.  PAREN-STATE is the remainder of the state of enclosing
  ;; braces
  ;;
  ;; This function might do hidden buffer changes.
  (or
   ;; This will pick up brace list declarations.
   (save-excursion
     (goto-char containing-sexp)
     (c-backward-over-enum-header))
   ;; this will pick up array/aggregate init lists, even if they are nested.
   (save-excursion
     (let (bufpos braceassignp lim next-containing macro-start)
       (while (and (not bufpos)
		   containing-sexp)
	 (when paren-state
	   (if (consp (car paren-state))
	       (setq lim (cdr (car paren-state))
		     paren-state (cdr paren-state))
	     (setq lim (car paren-state)))
	   (when paren-state
	     (setq next-containing (car paren-state)
		   paren-state (cdr paren-state))))
	 (goto-char containing-sexp)
	 (if (c-looking-at-inexpr-block next-containing next-containing)
	     ;; We're in an in-expression block of some kind.  Do not
	     ;; check nesting.  We deliberately set the limit to the
	     ;; containing sexp, so that c-looking-at-inexpr-block
	     ;; doesn't check for an identifier before it.
	     (setq containing-sexp nil)
	   ;; see if the open brace is preceded by = or [...]
           ;; or <...> in this statement
	   (setq braceassignp 'dontknow)
	   (c-backward-token-2 1 t lim)
	   ;; Checks to do only on the first sexp before the brace.
	   (when (and c-opt-inexpr-brace-list-key
		      (memq (char-after) '(?\[ ?\<)))
	     ;; In Apex, an initialization brace list may follow
	     ;; directly after "new Foo[] or List<...>", so check for a "new"
	     ;; earlier.
	     (while (eq braceassignp 'dontknow)
	       (setq braceassignp
		     (cond ((/= (c-backward-token-2 1 t lim) 0) nil)
			   ((looking-at c-opt-inexpr-brace-list-key) t)
			   ((looking-at "\\sw\\|\\s_\\|[.[<]")
			    ;; Carry on looking if this is an
			    ;; identifier (may contain "." in Apex)
			    ;; or another "[]" or "<> sexp.
			    'dontknow)
			   (t nil)))))
	   ;; Checks to do on all sexps before the brace, up to the
	   ;; beginning of the statement.
	   (while (eq braceassignp 'dontknow)
	     (cond ((eq (char-after) ?\;)
		    (setq braceassignp nil))
		   ((eq (char-after) ?=)
		    ;; We've seen a =, but must check earlier tokens so
		    ;; that it isn't something that should be ignored.
		    (setq braceassignp 'maybe)
		    (while (and (eq braceassignp 'maybe)
				(zerop (c-backward-token-2 1 t lim)))
		      (setq braceassignp
			    (cond ((looking-at "\\s.") 'maybe)
                                  (t t))))))
	     (if (and (eq braceassignp 'dontknow)
		      (/= (c-backward-token-2 1 t lim) 0))
		 (setq braceassignp nil)))
	   (cond
	    (braceassignp
	     ;; We've hit the beginning of the aggregate list.
	     (c-beginning-of-statement-1
	      (c-most-enclosing-brace paren-state))
	     (setq bufpos (point)))
	    ((eq (char-after) ?\;)
	     ;; Brace lists can't contain a semicolon, so we're done.
	     (setq containing-sexp nil))
	    (t
	     ;; Go up one level
	     (setq containing-sexp next-containing
		   lim nil
		   next-containing nil)))))

       bufpos))
   ))

(advice-add 'c-inside-bracelist-p :around
            (lambda (cc-fun &rest args)
              (if (eq major-mode 'apex-mode)
                  (apply 'apex-c-inside-bracelist-p args)
                (apply cc-fun args))))

;;;; customization

(defcustom apex-mode-keywords-case-fold nil
  "You can specify if keyword highlightning should be case-insensitive."
  :type '(choice (const :tag "Case-sensitive" nil)
                 (const :tag "Case-insensitive" t))
  :group 'apex-mode)

;;;; fonts

(defconst apex-font-lock-keywords-1
  (c-lang-const c-matchers-1 apex)
  "Minimal font locking for Apex mode.")

(defconst apex-font-lock-keywords-2
  (c-lang-const c-matchers-2 apex)
  "Fast normal font locking for Apex mode.")

(defconst apex-font-lock-keywords-3
  (c-lang-const c-matchers-3 apex)
  "Accurate normal font locking for Apex mode.")

(defvar apex-font-lock-keywords apex-font-lock-keywords-3
  "Default expressions to highlight in Apex mode.")

;;;; font-lock soql and sosl

(defvar apex-mode--soql-and-sosl-kwds-regexp
  (eval-when-compile
    (regexp-opt
     '("SELECT" "TYPEOF" "WHEN" "THEN" "ELSE" "END" "FROM" "USING"
       "SCOPE" "WHERE" "WITH" "DATA" "CATEGORY" "AT" "ABOVE" "BELOW"
       "ABOVE_OR_BELOW" "GROUP" "BY" "ROLLUP" "CUBE" "GROUPING" "HAVING"
       "ORDER" "ASC" "DESC" "NULLS" "FIRST" "LAST" "LIMIT" "OFFSET"
       "FOR" "VIEW" "REFERENCE" "UPDATE" "TRACKING" "VIEWSTAT" "TRUE"
       "FALSE" "LIKE" "IN" "NOT" "AND" "OR" "INCLUDES" "EXCLUDES"
       "AVG" "COUNT" "COUNT_DISTINCT" "MIN" "MAX" "SUM" "DISTANCE"
       "GEOLOCATION" "FORMAT" "CALENDAR_MONTH" "CALENDAR_QUARTER"
       "CALENDAR_YEAR" "DAY_IN_MONTH" "DAY_IN_WEEK" "DAY_IN_YEAR"
       "DAY_ONLY" "FISCAL_MONTH" "FISCAL_QUARTER" "FISCAL_YEAR"
       "HOUR_IN_DAY" "WEEK_IN_MONTH" "WEEK_IN_YEAR" "YESTERDAY" "TODAY"
       "TOMORROW" "LAST_WEEK" "THIS_WEEK" "NEXT_WEEK" "LAST_MONTH"
       "THIS_MONTH" "NEXT_MONTH" "LAST_90_DAYS" "NEXT_90_DAYS"
       "LAST_N_DAYS" "NEXT_N_DAYS" "LAST_N_WEEKS" "NEXT_N_WEEKS"
       "LAST_N_MONTHS" "NEXT_N_MONTHS" "THIS_QUARTER" "LAST_QUARTER"
       "NEXT_QUARTER" "LAST_N_QUARTERS" "NEXT_N_QUARTERS" "THIS_YEAR"
       "NEXT_YEAR" "LAST_YEAR" "LAST_N_YEARS" "NEXT_N_YEARS"
       "THIS_FISCAL_QUARTER" "LAST_FISCAL_QUARTER" "NEXT_FISCAL_QUARTER"
       "LAST_N_FISCAL_QUARTERS" "NEXT_N_FISCAL_QUARTERS" "THIS_FISCAL_YEAR"
       "LAST_FISCAL_YEAR" "NEXT_FISCAL_YEAR" "LAST_N_FISCAL_YEARS"
       "NEXT_N_FISCAL_YEARS" "FIND" "RETURNING" "SNIPPET"
       "NETWORK" "HIGHLIGHT" "METADATA" "ALL" "FIELDS" "EMAIL" "NAME"
       "PHONE" "SIDEBAR")
     'words)))

(defun apex-mode--soql-or-sosl-font-lock-matcher (limit)
  "Search for soql or sosl keywords not in comments."
  (let (res)
    (while
        (and (setq res (re-search-forward apex-mode--soql-and-sosl-kwds-regexp
                                          limit t))
             (eql (syntax-ppss-context (syntax-ppss)) 'comment)))
    res))

;;;; style

(c-add-style "apex" '("java" (c-offsets-alist . ((arglist-intro . *)
                                                 (statement-cont . *)))))

;;;; syntax-table

(defvar apex-mode-syntax-table nil
  "Syntax table used in apex-mode buffers.")
(unless apex-mode-syntax-table
  (setq apex-mode-syntax-table
        (funcall (c-lang-const c-make-mode-syntax-table apex)))
  (modify-syntax-entry ?_ "w" apex-mode-syntax-table))

;;;; abbrev-table

(defvar apex-mode-abbrev-table nil
  "Abbreviation table used in apex-mode buffers.")
(unless apex-mode-abbrev-table
  (setq apex-mode-abbrev-table
        (copy-abbrev-table java-mode-abbrev-table)))

;;;; map

(defvar apex-mode-map (c-make-inherited-keymap)
  "Keymap used in apex-mode buffers.")

;;;; menu

(easy-menu-define c-apex-menu apex-mode-map "Apex Mode Commands"
  (cons "Apex" (c-lang-const c-mode-menu apex)))

;;;; imenu

(defconst cc-imenu-apex-generic-expression
  cc-imenu-java-generic-expression
  "Imenu generic expression for Apex mode.  See `imenu-generic-expression'.")

;;;; soql and sosl special indent

(defun apex-mode--soql-select-align-clauses-regexp ()
  (eval-when-compile
    (regexp-opt
     '("FROM" "USING" "WHERE" "WITH" "GROUP" "ORDER" "LIMIT" "OFFSET" "FOR")
     'words)))

(defun apex-mode--sosl-find-align-clauses-regexp ()
  (eval-when-compile
    (regexp-opt '("IN" "RETURNING" "WITH" "LIMIT" "UPDATE") 'words)))

(defun apex-mode--soql-and-sosl-stmt-regexp ()
  (eval-when-compile
    (regexp-opt '("SELECT" "FIND") 'words)))

(defun apex-mode--soql-or-sosl-bracket-stmt-match (pos)
  (when (member (char-after pos) '(?\[ ?\())
    (save-excursion
      (goto-char (1+ pos))
      (forward-comment (point-max))
      (apex-mode--soql-or-sosl-stmt-match (point)))))

(defun apex-mode--soql-or-sosl-stmt-match (pos)
  (save-excursion
    (goto-char pos)
    (when (looking-at (apex-mode--soql-and-sosl-stmt-regexp))
      (intern (match-string-no-properties 0)))))

(defvar c-syntactic-context)

(defun apex-mode--soql-and-sosl-indent-hook ()
  (let* ((context (nreverse c-syntactic-context)) ; closest context
         (langelem (or (assq 'arglist-intro context)
                       (assq 'arglist-cont-nonempty context)
                       (assq 'arglist-cont context))))
    (when langelem
      (save-excursion
        (let ((sym (c-langelem-sym langelem))
              (anchor-pos (c-langelem-pos langelem))
              (bracket-pos (c-langelem-2nd-pos langelem))
              (org-indent (progn (back-to-indentation) (current-column)))
              new-indent match)
          (setq new-indent org-indent)
          (cond
           ;; var = [
           ;;     SELECT arg1,  <- c-basic-offset
           ((and (eq sym 'arglist-intro)
                 (apex-mode--soql-or-sosl-bracket-stmt-match bracket-pos))
            (setq new-indent (+ (c-langelem-col langelem t) c-basic-offset)))
           ;; [SELECT arg1,
           ;;      arg2         <- c-basic-offset
           ;;  FROM arg3
           ((and (eq sym 'arglist-cont-nonempty)
                 (setq match (apex-mode--soql-or-sosl-bracket-stmt-match bracket-pos)))
            (unless (or (and (eq match 'SELECT)
                             (looking-at-p (apex-mode--soql-select-align-clauses-regexp)))
                        (and (eq match 'FIND)
                             (looking-at-p (apex-mode--soql-find-align-clauses-regexp))))
              (setq new-indent (+ (current-column) c-basic-offset))))
           ;; var = [
           ;;     SELECT arg1,
           ;;         arg2      <- c-basic-offset
           ;;     FROM arg3
           ((and (eq sym 'arglist-cont)
                 (setq match (apex-mode--soql-or-sosl-stmt-match anchor-pos)))
            (unless (or (and (eq match 'SELECT)
                             (looking-at-p (apex-mode--soql-select-align-clauses-regexp)))
                        (and (eq match 'FIND)
                             (looking-at-p (apex-mode--soql-find-align-clauses-regexp))))
              (setq new-indent (+ (current-column) c-basic-offset)))))
          (c-shift-line-indentation (- new-indent org-indent)))))))

;;;; mode

;;;###autoload
(define-derived-mode apex-mode prog-mode "Apex"
  "Major mode for editing Apex code.

Key bindings:
\\{apex-mode-map}"
  (c-initialize-cc-mode t)
  (set-syntax-table apex-mode-syntax-table)
  (setq local-abbrev-table apex-mode-abbrev-table
        abbrev-mode t)
  (use-local-map apex-mode-map)
  (c-init-language-vars apex-mode)
  (c-common-init 'apex-mode)
  (unless (or c-file-style
              (stringp c-default-style)
              (assq 'apex-mode c-default-style))
    (c-set-style "apex"))
  (easy-menu-add c-apex-menu)
  (cc-imenu-init cc-imenu-apex-generic-expression)
  (setcar (nthcdr 2 font-lock-defaults) apex-mode-keywords-case-fold)
  (setq c-buffer-is-cc-mode 'java-mode)
  (add-hook 'c-special-indent-hook 'apex-mode--soql-and-sosl-indent-hook))

;;;; hooks

(defun apex-mode--font-lock-hook ()
  (font-lock-add-keywords
   nil
   `((apex-mode--soql-or-sosl-font-lock-matcher 0 font-lock-keyword-face prepend))
   'append ))

(add-hook 'apex-mode-hook 'apex-mode--font-lock-hook)

;;;###autoload
(setq auto-mode-alist
      (append
       '(("\\.cls\\'" . apex-mode)
         ("\\.trigger\\'" . apex-mode))
       auto-mode-alist))

(provide 'apex-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; apex-mode.el ends here
