; -*- Mode: Emacs-Lisp -*-

;;; apex-mode.el -- Major mode for Salesforce Apex files

;; Put this file into your Emacs lisp path (eg. site-lisp)
;; and add the following line to your ~/.emacs file:
;;
;;   (require 'apex-mode)
(require 'font-lock)

(defvar apex-mode-hook nil)

(defvar apex-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") 'newline-and-indent)
    keymap)
  "Keymap for Apex major mode")

(add-to-list 'auto-mode-alist '("\\.cls\\'" . apex-mode))

(add-to-list 'auto-mode-alist '("\\.trigger\\'" . apex-mode))

;; ;; the command to comment/uncomment text
;; (defun apex-comment-dwim (arg)
;; "Comment or uncomment current line or region in a smart way.
;; For detail, see `comment-dwim'."
;;    (interactive "*P")
;;    (require 'newcomment)
;;    (let ((deactivate-mark nil) (comment-start "/*") (comment-end "*/"))
;;      (comment-dwim arg)))

(define-derived-mode apex-mode java-mode "Apex"
  "Major mode for editing Salesforce Apex files"
  ;; (define-key apex-mode-map [remap comment-dwim] 'apex-comment-dwim)

;;  (set (make-local-variable 'font-lock-defaults) '(apex-font-lock-keywords nil t))
  )

(provide 'apex-mode)

;;; end of apex-mode.el
