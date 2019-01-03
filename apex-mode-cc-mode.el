;;; apex-mode-cc-mode.el --- cc-mode changes for Apex mode -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Magnus Nyberg

;; Author: Magnus Nyberg <magnus@nexter.se>
;; Keywords: apex, languages, force, sfdc, salesforce
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

;; Overrides and extends core cc-mode features and defines apex language
;; settings for Apex mode.

;;; Code:

;;;; cc-align

(require 'cc-align)

(defun apex-mode-c-block-in-arglist-dwim (_arglist-start)
  ;; This function implements the DWIM to avoid far indentation of
  ;; brace block constructs in arguments in `c-lineup-arglist' etc.
  ;; Return non-nil if a brace block construct is detected within the
  ;; arglist starting at ARGLIST-START.
  nil)

(advice-add 'c-block-in-arglist-dwim :around
            (lambda (cc-fun &rest args)
              (if (eq major-mode 'apex-mode)
                  (apply 'apex-mode-c-block-in-arglist-dwim args)
                (apply cc-fun args))))

;;;; cc-engine

(require 'cc-engine)

(defun apex-mode-c-cheap-inside-bracelist-p (_paren-state)
  ;; Return the position of the L-brace if point is inside a brace list
  ;; initialization of an array, etc.
  nil)

(advice-add 'c-cheap-inside-bracelist-p :around
            (lambda (cc-fun &rest args)
              (if (eq major-mode 'apex-mode)
                  (apply 'apex-mode-c-cheap-inside-bracelist-p args)
                (apply cc-fun args))))

(defun apex-mode--c-inside-bracelist-p (containing-sexp paren-state &rest _)
  ;; return the buffer position of the beginning of the brace list
  ;; statement if we're inside a brace list, otherwise return nil.
  ;; CONTAINING-SEXP is the buffer pos of the innermost containing
  ;; paren.  PAREN-STATE is the remainder of the state of enclosing
  ;; braces
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (let (bufpos braceassignp lim next-containing _macro-start)
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
          (setq braceassignp 'dontknow)
          (c-backward-token-2 1 t lim)
          ;; Checks to do only on the first sexp before the brace.
          (when (and c-opt-inexpr-brace-list-key
                     (memq (char-after) '(?\[ ?\<)))
            ;; In Apex, an initialization brace list may follow
            ;; directly after "new Foo[]" or "List<...>" etc.,
            ;; so check for a "new" earlier.
            (while (eq braceassignp 'dontknow)
              (setq braceassignp
                    (cond ((/= (c-backward-token-2 1 t lim) 0) nil)
                          ((looking-at c-opt-inexpr-brace-list-key) t)
                          ((looking-at "\\sw\\|\\s_\\|[.]")
                           ;; Carry on looking if this is an
                           ;; identifier (may contain "." in Apex)
                           'dontknow)
                          (t nil)))))
          (cond
           ((eq braceassignp t)
            (c-beginning-of-statement-1
             (c-most-enclosing-brace paren-state))
            (setq bufpos (point)))
           (t
            (setq containing-sexp nil)))))
      bufpos)))

(advice-add 'c-inside-bracelist-p :around
            (lambda (cc-fun &rest args)
              (if (eq major-mode 'apex-mode)
                  (apply 'apex-mode--c-inside-bracelist-p args)
                (apply cc-fun args))))

;;;; cc-langs

(require 'cc-langs)

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
         "update" "upsert" "switch" "when"))

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

(provide 'apex-mode-cc-mode)

;;; apex-mode-cc-mode.el ends here
