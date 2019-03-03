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

;; This is a major mode for editing Apex, SOQL and SOSL code.

;;; Code:

(eval-when-compile
  (require 'cc-mode))

(eval-and-compile
  (require 'apex-mode-cc-mode)
  (require 'cc-defs)
  (require 'cc-langs)
  (c-add-language 'apex-mode 'java-mode))

(require 'cc-engine)
(require 'cc-mode)
(require 'cc-vars)

;;;; customization

(defcustom apex-mode-keywords-case-fold nil
  "You can specify if keyword highlightning should be case-insensitive."
  :type '(choice (const :tag "Case-sensitive" nil)
                 (const :tag "Case-insensitive" t))
  :group 'c)

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
  :group 'c)

(defcustom apex-mode-hook nil
  "Hook called by `apex-mode'."
  :type 'hook
  :group 'c)

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

(c-add-style
 "apex"
 '("java"
   (c-offsets-alist . ((arglist-intro . *)
                       (arglist-close . c-lineup-close-paren)
                       (statement-cont . *)
                       (else-clause . apex-mode--lineup-else-after-when)))))

(defun apex-mode--switch-when-clauses-regexp ()
  (eval-when-compile
    (regexp-opt '("when") 'words)))

(defvar c-syntactic-element)

(defun apex-mode--lineup-else-after-when (langelem)
  (let ((pos (c-langelem-pos c-syntactic-element)))
    (if (and pos (save-excursion
                   (goto-char pos)
                   (looking-at-p (apex-mode--switch-when-clauses-regexp))))
        (c-calc-offset '(statement-cont))
      (let ((sym (c-langelem-sym langelem)))
        (c-evaluate-offset
         (cdr (assq sym (assq 'c-offsets-alist c-fallback-style)))
         langelem sym)))))

;;;; syntax-table

(defvar apex-mode-syntax-table nil
  "Syntax table used in apex-mode buffers.")
(unless apex-mode-syntax-table
  (setq apex-mode-syntax-table
        (funcall (c-lang-const c-make-mode-syntax-table apex)))
  (modify-syntax-entry ?_ "w" apex-mode-syntax-table)
  (modify-syntax-entry ?\" "." apex-mode-syntax-table))

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

(eval-when-compile
  (defconst cc-imenu-apex-generic-expression
    cc-imenu-java-generic-expression
    "Imenu generic expression for Apex mode.  See `imenu-generic-expression'."))

;;;; mode

;;;###autoload
(define-derived-mode apex-mode prog-mode "Apex"
  "Major mode for editing Apex code.

Key bindings:
\\{apex-mode-map}"
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (setq abbrev-mode t)
  (use-local-map apex-mode-map)
  (c-init-language-vars apex-mode)
  (c-common-init 'apex-mode)
  (unless (or c-file-style
              (stringp c-default-style)
              (assq 'apex-mode c-default-style))
    (c-set-style "apex"))
  (easy-menu-add c-apex-menu)
  (eval-when-compile
    (require 'cc-menus)
    (cc-imenu-init cc-imenu-apex-generic-expression))
  (setcar (nthcdr 2 font-lock-defaults) apex-mode-keywords-case-fold)
  (c-run-mode-hooks 'c-mode-common-hook))

;;;; submodes

(require 'submode)

(require 'soql-mode)
(defconst apex-mode--soql-submode
  (submode-construct-submode
   'soql-mode
   :name "(SOQL)"
   :relative-indent 'block
   :syntax-propertize-rules
   (syntax-propertize-precompile-rules
    ("\\\[[ \t\n]*SELECT[ \t\n]"
     (0 (ignore
         (goto-char (1+ (match-beginning 0)))
         (submode-syntax-propertize apex-mode--soql-submode end)))))
   :end-tag "]"
   :syntax-table soql-mode-syntax-table
   :keymap soql-mode-map))

(require 'sosl-mode)
(defconst apex-mode--sosl-submode
  (submode-construct-submode
   'sosl-mode
   :name "(SOSL)"
   :relative-indent 'block
   :syntax-propertize-rules
   (syntax-propertize-precompile-rules
    ("\\\[[ \t\n]*FIND[ \t\n]"
     (0 (ignore
         (goto-char (1+ (match-beginning 0)))
         (submode-syntax-propertize apex-mode--sosl-submode end)))))
   :end-tag "]"
   :syntax-table sosl-mode-syntax-table
   :keymap sosl-mode-map))

(add-hook
 'apex-mode-hook
 (lambda ()
   (submode-construct-main-mode
    :name '("Apex" (:eval (submode-lighter)))
    :case-fold-search apex-mode-keywords-case-fold
    :submodes '(apex-mode--soql-submode apex-mode--sosl-submode)
    :config
    '((c-update-modeline)               ; Refresh mode-name
      ;; Let submode case-fold follow main-mode
      (setq soql-mode-keywords-case-fold apex-mode-keywords-case-fold)
      (setq sosl-mode-keywords-case-fold apex-mode-keywords-case-fold)))))

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
