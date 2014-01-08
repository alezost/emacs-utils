;;; utl-w3m.el --- Additional functionality for w3m

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 24 Sep 2013

;;; Code:

(defun utl-w3m-next-url ()
  "Go to the next page."
  (interactive)
  (if w3m-next-url
      (let ((w3m-prefer-cache t))
        (w3m-history-store-position)
        (w3m-goto-url w3m-next-url))
    (message "No 'next' link found.")))

(defun utl-w3m-previous-url ()
  "Go to the previous page."
  (interactive)
  (if w3m-previous-url
      (let ((w3m-prefer-cache t))
        (w3m-history-store-position)
        (w3m-goto-url w3m-previous-url))
    (message "No 'previous' link found.")))

(provide 'utl-w3m)

;;; utl-w3m.el ends here
