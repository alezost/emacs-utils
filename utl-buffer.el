;;; utl-buffer.el --- Additional functionality for working with buffers

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 10 Aug 2013

;;; Code:

(defun utl-kill-file-buffer (filename)
  "Kill the buffer visiting file FILENAME (a string).
Return nil, if there is no such live buffer."
  (let ((buffer (get-file-buffer filename)))
    (if buffer (kill-buffer buffer))))

(defun utl-re-buffer-list (regexp)
  "Return a list of buffers which names match REGEXP."
  (let ((buffers))
    (dolist (buffer (buffer-list) buffers)
      (if (string-match-p regexp (buffer-name buffer))
          (setq buffers (cons buffer buffers))))))

;;;###autoload
(defun utl-file-name-to-kill-ring ()
  "Put a name of the file visited by the current buffer into kill ring."
  (interactive)
  (if (null buffer-file-name)
      (message "buffer-file-name is nil (not visiting a file)")
    (kill-new buffer-file-name)
    (message buffer-file-name)))

(defun utl-switch-to-buffer-or-funcall (buffer &optional fun)
  "Switch to the buffer BUFFER-OR-NAME or call a function FUN.
BUFFER-OR-NAME can be a string, a buffer object or a function
returning one of those.  If there is no such buffer, call a
function FUN if it is specified."
  (let ((buf (get-buffer (if (functionp buffer)
                             (funcall buffer)
                           buffer))))
    (if buf
        (switch-to-buffer buf)
      (and fun (funcall fun)))))

(provide 'utl-buffer)

;;; utl-buffer.el ends here
