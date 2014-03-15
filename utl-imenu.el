;;; utl-imenu.el --- Additional functionality for imenu

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 15 Mar 2014

;;; Code:

;; If you have sections in lisp/elisp files that begin with ";;;", you
;; may use the following code to add "Sections" entry in `imenu':
;;
;; (add-hook 'emacs-lisp-mode-hook 'utl-imenu-add-sections)
;; (add-hook 'lisp-mode-hook 'utl-imenu-add-sections)

(defvar utl-imenu-sections-re "^;;; \\(.+\\)$"
  "Regexp for sections in (e)lisp files.")

;;;###autoload
(defun utl-imenu-add-sections ()
  "Add `utl-imenu-sections-re' to `imenu-generic-expression'."
  (add-to-list
   'imenu-generic-expression
   (list "Sections" utl-imenu-sections-re 1)
   t))

(provide 'utl-imenu)

;;; utl-imenu.el ends here
