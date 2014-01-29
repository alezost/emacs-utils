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


;;; Download with wget

(defvar utl-w3m-download-dir nil
  "Default directory for downloading with wget.")

;; from <http://www.emacswiki.org/emacs/WThreeMHintsAndTips>
(defun utl-w3m-download-with-wget (dir)
  "Download url at point to the directory DIR with wget."
  (interactive
   (list (ido-read-directory-name "Save to: " utl-w3m-download-dir)))
  (let ((url (or (w3m-anchor) (w3m-image))))
    (if url
	(let ((proc (start-process "wget" (format "*wget %s*" url)
				   "wget" "--passive-ftp" "-nv"
				   "-P" (expand-file-name dir) url)))
	  (set-process-sentinel proc
                                (lambda (proc str)
                                  (message "Download finished."))))
      (message "Nothing to get."))))

(provide 'utl-w3m)

;;; utl-w3m.el ends here
