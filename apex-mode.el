;;; apex-mode.el --- Salesforce Apex Major Mode   -*- lexical-binding: t -*-

;; Copyright (C) 2011-2018 Magnus Nyberg

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

(eval-when-compile
  (require 'cc-mode))

(eval-and-compile
  (require 'apex-mode-cc-mode)
  (c-add-language 'apex-mode 'java-mode))

;;;; customization

(defcustom apex-mode-keywords-case-fold nil
  "You can specify if keyword highlightning should be case-insensitive."
  :type '(choice (const :tag "Case-sensitive" nil)
                 (const :tag "Case-insensitive" t))
  :group 'apex-mode)

(defcustom apex-font-lock-extra-types
  (list (concat "[" c-upper "]\\sw*[" c-lower "]\\sw*"))
  (c-make-font-lock-extra-types-blurb
   "Apex" "apex-mode"
   (concat
    "For example, a value of "
    "(\"[" c-upper "]\\\\sw*[" c-lower "]\\\\sw*\") means\n"
    "capitalized words are treated as type names (the requirement for a\n"
    "lower case char is to avoid recognizing all-caps constant names)."))
  :type 'c-extra-types-widget
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
