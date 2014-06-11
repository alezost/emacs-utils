;;; utl-yasnippet.el --- Additional functionality for yasnippet

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 7 Jun 2014

;;; Code:

(require 'yasnippet)

;;;###autoload
(defun utl-yas-next-field-or-expand ()
  "Go to the next field if a snippet is in progress or perform an expand."
  (interactive)
  (if (yas--snippets-at-point 'all)
      (goto-char (overlay-start yas--active-field-overlay))
    (yas-expand)))

(defadvice yas--on-protection-overlay-modification
    (around utl-violate-fields-modification)
  "Allow any editing during working with a snippet."
  (let ((yas--inhibit-overlay-hooks t))
    ad-do-it))

(provide 'utl-yasnippet)

;;; utl-yasnippet.el ends here
