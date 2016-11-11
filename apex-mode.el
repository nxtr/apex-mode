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

(defvar apex-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") 'newline-and-indent)
    keymap)
  "Keymap for Apex major mode")

(add-to-list 'auto-mode-alist '("\\.cls\\'" . apex-mode))
(add-to-list 'auto-mode-alist '("\\.trigger\\'" . apex-mode))

(define-derived-mode apex-mode java-mode "Apex"
  "Major mode for editing Salesforce Apex files")

(defun my-apex-mode-hook ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'topmost-intro-cont 0)  ; dirty fix for apex coll initializer
  #'linum-on)

(add-hook 'apex-mode-hook 'my-apex-mode-hook)

(add-to-list 'load-path (expand-file-name "~/salesforce"))
(require 'salesforce)

(add-to-list
 'compilation-error-regexp-alist-alist
 '(ap
   "[0-9]+\.  \\(.+?\\) -- Error:.+?(line \\([0-9]+\\), column \\([0-9]+\\))$" 1 2 3))

(provide 'apex-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; apex-mode.el ends here
