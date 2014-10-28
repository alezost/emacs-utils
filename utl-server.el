;;; utl-server.el --- Additional functionality for working with emacs servers

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 21 Jun 2014

;;; Code:

(require 'server)

(defvar utl-server-name-fallback "server-unused"
  "Default server name used by `utl-server-start'.")

;;;###autoload
(defun utl-server-named-start (names)
  "Start server using the first server name from NAMES.
If there is such server running, try the second name and so on.
If servers with all NAMES are running, set `server-name' to
`utl-fallback-server-name' and do not start it."
  (let ((name (car names))
        (rest-names (cdr names)))
    (if (null name)
        (setq server-name utl-server-name-fallback)
      (setq server-name name)
      (if (server-running-p)
          (utl-server-named-start rest-names)
        (utl-server-start)))))


;;; Server name in the mode line

;; To have a server name of the running server in the mode-line, I use
;; an auxiliary variable `utl-server-is-running', because calling of
;; `server-running-p' in the mode-line construct eats CPU.

(defvar utl-server-is-running nil
  "The state of the current server.
This variable is set by `utl-server-start'.")

;; Idea of right-aligning from <http://lists.gnu.org/archive/html/help-gnu-emacs/2013-12/msg00191.html>
(defvar utl-mode-server
  '(utl-server-is-running
    (:eval (list (propertize " " 'display
                             `(space :align-to (- right ,(length server-name))))
                 server-name)))
  "Mode line construct for displaying `server-name' if server is running.
This variable is intended to be used in `mode-line-format' or
some inner mode line variable.")
(put 'utl-mode-server 'risky-local-variable t)

;;;###autoload
(defun utl-server-start (&optional leave-dead inhibit-prompt)
  "Same as `server-start' but also set `utl-server-is-running'."
  (interactive "P")
  (server-start leave-dead inhibit-prompt)
  (setq utl-server-is-running (not leave-dead)))

(provide 'utl-server)

;;; utl-server.el ends here
