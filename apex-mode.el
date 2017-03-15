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

(c-lang-defconst c-after-id-concat-ops
  "Operators that can occur after a binary operator on `c-identifier-ops'
in identifiers.  nil in languages that don't have such things.

Operators here should also have appropriate entries in `c-operators' -
it's not taken care of by default."
  apex nil)

(c-lang-defconst c-assignment-operators
  "List of all assignment operators."
  apex '("=" "*=" "/=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|=" ">>>="))

(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Do not try to modify this list for end user customizations; the
`*-font-lock-extra-types' variable, where `*' is the mode prefix, is
the appropriate place for that."
  apex '("after" "before" "void"))

(c-lang-defconst c-type-prefix-kwds
  "Keywords where the following name - if any - is a type name, and
where the keyword together with the symbol works as a type in
declarations.

Note that an alternative if the second part doesn't hold is
`c-type-list-kwds'.  Keywords on this list are typically also present
on one of the `*-decl-kwds' lists."
  apex '("on"))

(c-lang-defconst c-class-decl-kwds
  "Keywords introducing declarations where the following block (if any)
contains another declaration level that should be considered a class.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Note that presence on this list does not automatically treat the
following identifier as a type; the keyword must also be present on
`c-type-prefix-kwds' or `c-type-list-kwds' to accomplish that."
  apex '("class" "interface" "trigger"))

(c-lang-defconst c-typeless-decl-kwds
  "Keywords introducing declarations where the \(first) identifier
\(declarator) follows directly after the keyword, without any type.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  ;; Default to `c-class-decl-kwds' and `c-brace-list-decl-kwds'
  ;; (since e.g. "Foo" is the identifier being defined in "class Foo
  ;; {...}").
  apex (append (c-lang-const c-class-decl-kwds)
               (c-lang-const c-brace-list-decl-kwds)
               '("get" "set")))

(c-lang-defconst c-modifier-kwds
  "Keywords that can prefix normal declarations of identifiers
\(and typically act as flags).  Things like argument declarations
inside function headers are also considered declarations in this
sense.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  apex '("abstract" "final" "global" "override" "private" "protected" "public"
         "with sharing" "without sharing" "static" "testmethod" "transient"
         "virtual" "webservice"))

(c-lang-defconst c-other-decl-kwds
  "Keywords that can start or prefix any declaration level construct,
besides those on `c-class-decl-kwds', `c-brace-list-decl-kwds',
`c-other-block-decl-kwds', `c-typedef-decl-kwds',
`c-typeless-decl-kwds' and `c-modifier-kwds'.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
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
element.)

Assumed to be mutually exclusive with `c-ref-list-kwds'.  There's no
reason to put keywords on this list if they are on `c-type-prefix-kwds'.
There's also no reason to add keywords that prefixes a normal
declaration consisting of a type followed by a declarator (list), so
the keywords on `c-modifier-kwds' should normally not be listed here
either.

Note: Use `c-typeless-decl-kwds' for keywords followed by a function
or variable identifier (that's being defined)."
  apex '("extends" "implements"))

(c-lang-defconst c-ref-list-kwds
  "Keywords that may be followed by a comma separated list of
reference (i.e. namespace/scope/module) identifiers, where each
optionally can be prefixed by keywords.  (Can also be used for the
special case when the list can contain only one element.)  Assumed to
be mutually exclusive with `c-type-list-kwds'.

Note: Use `c-typeless-decl-kwds' for keywords followed by a function
or variable identifier (that's being defined)."
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
the open brace in any construct that contains a brace block, e.g. in
the \"class Foo: public Bar\" part of:

    class Foo: public Bar {int x();} a, *b;

If parens can occur, the chars inside those aren't filtered with this
list.

`<' and `>' should be disallowed even if angle bracket arglists can
occur.  That since the search function needs to stop at them anyway to
ensure they are given paren syntax.

This is used to skip backward from the open brace to find the region
in which to look for a construct like \"class\", \"enum\",
\"namespace\" or whatever.  That skipping should be as tight as
possible for good performance."
  ;; Allow ',' for multiple inherits.
  apex (c--set-difference (c-lang-const c-block-prefix-disallowed-chars)
                          '(?,)))

(c-lang-defconst c-recognize-typeless-decls
  "Non-nil means function declarations without return type should be
recognized.  That can introduce an ambiguity with parenthesized macro
calls before a brace block.  This setting does not affect declarations
that are preceded by a declaration starting keyword, so
e.g. `c-typeless-decl-kwds' may still be used when it's set to nil."
  apex nil)

(c-lang-defconst c-enums-contain-decls
  "Non-nil means that an enum structure can contain declarations."
  apex nil)

(c-lang-defconst c-recognize-colon-labels
  "Non-nil if generic labels ending with \":\" should be recognized.
That includes labels in code and access keys in classes.  This does
not apply to labels recognized by `c-label-kwds' and
`c-opt-extra-label-key'."
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
  ;; N.B.: This algorithm can potentially get confused by cpp macros
  ;; placed in inconvenient locations.  It's a trade-off we make for
  ;; speed.
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

;;;; style

(c-add-style "apex" '("java" (c-offsets-alist . ((arglist-intro . *)
                                                 (statement-cont . *)))))

;;;; mode

;;;###autoload
(define-derived-mode apex-mode java-mode "Apex"
  "Major mode for editing Apex code.

Key bindings:
\\{apex-mode-map}"
  (c-initialize-cc-mode t)
  (c-init-language-vars apex-mode)
  (unless (or c-file-style
              (stringp c-default-style)
              (assq 'apex-mode c-default-style))
    (c-set-style "apex"))
  (c-common-init 'apex-mode)
  (setcar (nthcdr 2 font-lock-defaults) apex-mode-keywords-case-fold)
  (setq c-buffer-is-cc-mode 'java-mode))

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
