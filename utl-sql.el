;;; utl-sql.el --- Additional functionality for sql stuff

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 22 Nov 2013

;;; Code:

(require 'sql)
(require 'cl-macs)


;;; Log of sql commands

;; Idea from <http://www.emacswiki.org/emacs/SqlMode>.  Add to .emacs:
;; (add-hook 'sql-interactive-mode-hook 'utl-sql-save-history)

(defcustom utl-sql-history-dir
  (expand-file-name "sql" user-emacs-directory)
  "Directory for history of sql commands."
  :type 'string
  :group 'sql)

;;;###autoload
(defun utl-sql-save-history ()
  "Save a history of commands separately for each sql-product.
Use `utl-sql-history-dir'."
  (if sql-product
      (setq-local sql-input-ring-file-name
                  (expand-file-name (concat (symbol-name sql-product)
                                            "-history.sql")
                                    utl-sql-history-dir))
    (error "SQL history will not be saved because sql-product is nil")))


;;; Mode line

(defvar utl-mode-info)

(defun utl-sql-highlight-product ()
  "Add sql product name to `utl-mode-info' instead of `mode-name'.
This function is intended to be used as a substitution for
`sql-highlight-product'."
  (when (derived-mode-p 'sql-mode)
    (set-syntax-table (sql-product-syntax-table))
    (sql-product-font-lock nil t))
  (and (or (derived-mode-p 'sql-mode)
           (derived-mode-p 'sql-interactive-mode))
       (setq utl-mode-info
             (or (sql-get-product-feature sql-product :name)
                 (symbol-name sql-product)))))

(provide 'utl-sql)

;;; utl-sql.el ends here
