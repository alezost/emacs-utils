;;; utl-server.el --- Additional functionality for working with emacs servers

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 21 Jun 2014

;;; Code:

(require 'server)

(defvar utl-server-name-fallback "server-unused"
  "Default server name used by `utl-server-start'.")

;;;###autoload
(defun utl-server-start (names)
  "Start server with the first name from NAMES.
If there is such server running, try the second name and so on.
If servers with all NAMES are running, set `server-name' to
`utl-fallback-server-name' and do not start it."
  (let ((name (car names))
        (rest-names (cdr names)))
    (if (null name)
        (setq server-name utl-server-name-fallback)
      (setq server-name name)
      (if (server-running-p)
          (utl-server-start rest-names)
        (server-start)))))

(provide 'utl-server)

;;; utl-server.el ends here
