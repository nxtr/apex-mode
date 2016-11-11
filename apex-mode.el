;;; apex-mode.el -- Major mode for Salesforce Apex files

;; Copyright (C) 2016 Magnus Nyberg

;; Author: Magnus Nyberg <magnus@nexter.se>
;; Keywords: languages, apex, force, sfdc, salesforce

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
  t nil)

(c-lang-defconst c-assignment-operators
  "List of all assignment operators."
  t '("=" "*=" "/=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|=" ">>>="))

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
  t '("after" "before" "void"))

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
  t '("class" "interface" "trigger"))

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
  t (append (c-lang-const c-class-decl-kwds)
            (c-lang-const c-brace-list-decl-kwds)
            '("get" "on" "set")))

(c-lang-defconst c-modifier-kwds
  "Keywords that can prefix normal declarations of identifiers
\(and typically act as flags).  Things like argument declarations
inside function headers are also considered declarations in this
sense.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  t '("abstract" "final" "global" "override" "private" "protected" "public"
      "with sharing" "without sharing" "static" "testmethod" "transient"
      "virtual" "webservice"))

(c-lang-defconst c-postfix-decl-spec-kwds
  "Keywords introducing extra declaration specifiers in the region
between the header and the body \(i.e. the \"K&R-region\") in
declarations."
  t '("extends" "implements"))

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
  t '("extends" "implements"))

(c-lang-defconst c-ref-list-kwds
  "Keywords that may be followed by a comma separated list of
reference (i.e. namespace/scope/module) identifiers, where each
optionally can be prefixed by keywords.  (Can also be used for the
special case when the list can contain only one element.)  Assumed to
be mutually exclusive with `c-type-list-kwds'.

Note: Use `c-typeless-decl-kwds' for keywords followed by a function
or variable identifier (that's being defined)."
  t nil)

(c-lang-defconst c-block-stmt-1-2-kwds
  "Statement keywords optionally followed by a paren sexp.
Keywords here should also be in `c-block-stmt-1-kwds'."
  t nil)

(c-lang-defconst c-block-stmt-2-kwds
  "Statement keywords followed by a paren sexp and then by a substatement."
  t '("for" "if" "while" "catch"))

(c-lang-defconst c-simple-stmt-kwds
  "Statement keywords followed by an expression or nothing."
  t '("break" "continue" "delete" "insert" "merge" "return" "throw" "undelete"
      "update" "upsert"))

(c-lang-defconst c-case-kwds
  "The keyword(s) which introduce a \"case\" like construct.
This construct is \"<keyword> <expression> :\"."
  t nil)

(c-lang-defconst c-label-kwds
  "Keywords introducing colon terminated labels in blocks."
  t nil)

(c-lang-defconst c-before-label-kwds
  "Keywords that might be followed by a label identifier."
  t nil)

(c-lang-defconst c-inexpr-class-kwds
  "Keywords that can start classes inside expressions."
  t '("new"))

(c-lang-defconst c-inexpr-brace-list-kwds
  "Keywords that can start brace list blocks inside expressions.
Note that Java specific rules are currently applied to tell this from
`c-inexpr-class-kwds'."
  t '("new"))

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
  t (c--set-difference (c-lang-const c-block-prefix-disallowed-chars)
                       '(?,)))

(c-lang-defconst c-recognize-typeless-decls
  "Non-nil means function declarations without return type should be
recognized.  That can introduce an ambiguity with parenthesized macro
calls before a brace block.  This setting does not affect declarations
that are preceded by a declaration starting keyword, so
e.g. `c-typeless-decl-kwds' may still be used when it's set to nil."
  t nil)

(c-lang-defconst c-enums-contain-decls
  "Non-nil means that an enum structure can contain declarations."
  t nil)

(c-lang-defconst c-recognize-colon-labels
  "Non-nil if generic labels ending with \":\" should be recognized.
That includes labels in code and access keys in classes.  This does
not apply to labels recognized by `c-label-kwds' and
`c-opt-extra-label-key'."
  t nil)

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

;;;; mode

;;;###autoload
(define-derived-mode apex-mode java-mode "Apex"
  "Major mode for editing Apex code.
Key bindings:
\\{apex-mode-map}"
  (c-initialize-cc-mode t)
  (c-init-language-vars apex-mode)
  (c-common-init 'apex-mode)
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
