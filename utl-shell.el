;;; utl-shell.el --- Additional functionality for shell

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 13 Jun 2015

;;; Code:

(require 'shell)

;; This function is originally taken from
;; <https://github.com/thierryvolpiatto/pcomplete-extension/blob/master/pcomplete-extension.el>
;; (`shell-command-completion-function').
(defun utl-shell-command-completion ()
  "Completion function for shell command names.
This function is intended to be used as a value of
`pcomplete-command-completion-function' variable for shell
buffers.  It allows to complete a command name even if it is not
the first command."
  (let* ((data (shell--command-completion-data))
         (input (and data (buffer-substring-no-properties
                           (nth 0 data) (nth 1 data)))))
    (if (and data (not (string-match-p "\\`[.]\\{1\\}/" input)))
        (pcomplete-here (all-completions "" (nth 2 data)))
      (shell-command-completion-function))))

(provide 'utl-shell)

;;; utl-shell.el ends here
