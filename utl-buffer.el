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

;;;###autoload
(defun utl-major-mode-to-kill-ring ()
  "Put major mode name of the current buffer into kill ring."
  (interactive)
  (let ((mode (symbol-name major-mode)))
    (kill-new mode)
    (message mode)))


;;; Switching to some buffers

(defun utl-switch-to-buffer-or-funcall (buffer &optional fun)
  "Switch to the buffer BUFFER-OR-NAME or call a function FUN.
BUFFER-OR-NAME can be a string, a buffer object or a function
returning one of those.  If there is no such buffer, call a
function FUN if it is specified."
  (let ((buf (if (functionp buffer)
                 (funcall buffer)
               buffer)))
    (if (and buf
             (setq buf (get-buffer buf)))
        (switch-to-buffer buf)
      (and fun (funcall fun)))))

;;;###autoload
(defun utl-switch-to-characters ()
  "Switch to the buffer with unicode characters."
  (interactive)
  (utl-switch-to-buffer-or-funcall
   "*Character List*"
   (lambda () (list-charset-chars 'unicode-bmp))))

;;;###autoload
(defun utl-switch-to-packages ()
  "Switch to the buffer with packages."
  (interactive)
  (utl-switch-to-buffer-or-funcall
   "*Packages*" #'list-packages))

;;;###autoload
(defun utl-switch-to-w3m ()
  "Switch to the `w3m' buffer."
  (interactive)
  (utl-switch-to-buffer-or-funcall
   (lambda ()
     (if (fboundp 'w3m-alive-p)
         (w3m-alive-p)
       (error "w3m is not running")))
   #'w3m))

;;;###autoload
(defun utl-switch-to-aurel-list ()
  "Switch to the `aurel-list-buffer-name' buffer."
  (interactive)
  (utl-switch-to-buffer-or-funcall
   aurel-list-buffer-name #'aurel-package-search))

;;;###autoload
(defun utl-switch-to-aurel-info ()
  "Switch to the `aurel-info-buffer-name' buffer."
  (interactive)
  (utl-switch-to-buffer-or-funcall
   aurel-info-buffer-name #'aurel-package-info))

(provide 'utl-buffer)

;;; utl-buffer.el ends here
