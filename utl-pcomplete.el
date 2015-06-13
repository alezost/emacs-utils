;;; utl-pcomplete.el --- Additional functionality for pcomplete

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 9 Jun 2015

;;; Code:

(require 'pcomplete)

(defvar utl-pcomplete-skipped-commands
  '("sudo" "xargs")
  "List of special commands for `utl-pcomplete-reduce-args-maybe'.")

(defun utl-pcomplete-reduce-args-maybe (&rest _)
  "Change some global variables to complete a special command properly.
If a command from `utl-pcomplete-skipped-commands' is being
completed, skip it and perform completion as if the next argument
was the current command."
  (when pcomplete-args
    (let ((cmd (file-name-nondirectory (car pcomplete-args))))
      (when (member cmd utl-pcomplete-skipped-commands)
        (setq pcomplete-args (cdr pcomplete-args)
              pcomplete-last (1- pcomplete-last))))))

(defun utl-pcomplete-no-space ()
  "Do not terminate a completion with space in the current buffer."
  (setq-local pcomplete-termination-string ""))

(provide 'utl-pcomplete)

;;; utl-pcomplete.el ends here
