;;; utl-header2.el --- Additional functionality for header2

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 23 Dec 2013

;;; Code:

(defcustom utl-auto-header-file-path-regexps nil
  "List of regexps of absolute file paths for auto-updating headers."
  :group 'header2
  :type  '(repeat string))

(defun utl-enable-auto-header-maybe ()
  "Enable auto-updating headers for the current file if needed.
Enable updating only if the file path match one of
`utl-auto-header-file-path-regexps'."
  (let ((path (buffer-file-name)))
    (and (cl-some (lambda (re)
                    (string-match re path))
                  utl-auto-header-file-path-regexps)
         (setq-local header-auto-update-enabled t))))

(provide 'utl-header2)

;;; utl-header2.el ends here
