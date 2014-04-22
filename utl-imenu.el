;;; utl-imenu.el --- Additional functionality for imenu

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 15 Mar 2014

;;; Code:

(require 'imenu)

;; If you have sections in lisp/elisp files that begin with ";;;", you
;; may use the following code to add "Sections" entry in `imenu':
;;
;; (add-hook 'emacs-lisp-mode-hook 'utl-imenu-add-sections)
;; (add-hook 'lisp-mode-hook 'utl-imenu-add-sections)

(defvar utl-imenu-sections-re "^;;; \\(.+\\)$"
  "Regexp used for \"Sections\" imenu entries.")

;;;###autoload
(defun utl-imenu-add-sections (&optional regexp)
  "Add REGEXP as a \"Sections\" element to `imenu-generic-expression'.
If REGEXP is nil, use `utl-imenu-sections-re'."
  (add-to-list
   'imenu-generic-expression
   (list "Sections" (or regexp utl-imenu-sections-re) 1)
   t))

;; To have "Sections" imenu entry (lines beginning with "///" in this
;; example) in javascript buffers, I use the following:
;;
;; (add-hook 'js-mode-hook
;;           (lambda ()
;;             (utl-imenu-add-sections "^/// \\(.+\\)$")
;;             (setq imenu-create-index-function 'utl-js-imenu-create-index)))

(declare-function js--imenu-create-index "js" nil)

;;;###autoload
(defun utl-js-imenu-create-index ()
  "Create an index alist for the current js buffer.
The function is suitable for `imenu-create-index-function'
variable and intended to be used instead of
`js--imenu-create-index' in js buffers.  It adds the same entries
as the latter function and also create elements for
`imenu-generic-expression'."
  (let ((js-index (js--imenu-create-index))
        (generic-index
         (save-excursion
           (save-restriction
             (widen)
             (imenu--generic-function imenu-generic-expression)))))
    (append js-index generic-index)))

(provide 'utl-imenu)

;;; utl-imenu.el ends here
