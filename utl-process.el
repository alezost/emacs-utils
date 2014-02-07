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

;; idea from <http://stackoverflow.com/questions/11572934/how-do-i-kill-a-running-process-in-emacs>
;;;###autoload
(defun utl-kill-process (process)
  "Kill PROCESS.
See `delete-process' for the meaning of PROCESS.
Interactively prompt for PROCESS name."
  (interactive
   (list (get-process (ido-completing-read
                       "Kill process: "
                       (mapcar 'process-name (process-list))))))
  (delete-process process))

(provide 'utl-process)

;;; utl-process.el ends here
