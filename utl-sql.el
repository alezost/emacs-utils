;;; utl-sql.el --- Additional functionality for sql stuff

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 22 Nov 2013

;;; Code:

(require 'sql)
(require 'cl-macs)

;;;###autoload
(defun utl-sql-connect (product database user)
  "Connect to an SQL PRODUCT using DATABASE and USER.
Do not prompt for anything else."
  (let ((sql-database database)
        (sql-user user)
        (login-var (sql-get-product-feature
                    product :sqli-login nil t)))
    (cl-letf (((symbol-value login-var) nil))
      (sql-product-interactive product))))

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

(provide 'utl-sql)

;;; utl-sql.el ends here
