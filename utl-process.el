;;; utl-process.el --- Additional functionality for working with processs

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 10 Aug 2013

;;; Code:

(defun utl-start-process (program &rest args)
  "Same as `start-process', but don't bother about name and buffer."
  (let ((process-name (concat program "_process"))
        (buffer-name  (generate-new-buffer-name
                       (concat program "_output"))))
    (apply 'start-process
           process-name buffer-name program args)))

(provide 'utl-process)

;;; utl-process.el ends here
