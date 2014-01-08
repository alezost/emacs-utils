;;; utl-browse-url.el --- Additional functionality for browse-url package

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 24 Sep 2013

;;; Code:

(require 'browse-url)

;;; Add support for the Conkeror browser.

(defcustom utl-browse-url-conkeror-program "conkeror"
  "The name by which to invoke Conkeror."
  :type 'string
  :group 'browse-url)

(defcustom utl-browse-url-conkeror-arguments nil
  "A list of strings to pass to Conkeror as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

;;;###autoload
(defun utl-browse-url-conkeror (url &optional new-window)
  "Ask the Conkeror WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `utl-browse-url-conkeror-arguments' are also passed to
Conkeror."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat "conkeror " url) nil
	   utl-browse-url-conkeror-program
	   (append
	    utl-browse-url-conkeror-arguments
	    (list url)))))



;;;###autoload
(defun utl-choose-browser (url &rest args)
  "Choose a browser for openning URL."
  (interactive "sURL: ")
  (let ((choice (read-char
                 (format "Choose a browser for '%s'\n(c - conkeror (default), f - firefox, w - w3m): "
                         url))))
    (funcall
     (cond
      ((or (equal choice ?c)
           (equal choice 13)) 'utl-browse-url-conkeror)
      ((equal choice ?f)      'browse-url-firefox)
      ((equal choice ?w)      'w3m-browse-url)
      (t (error "Wrong answer, use 'c', 'f' or 'w'")))
     url)))

(provide 'utl-browse-url)

;;; utl-browse-url.el ends here
