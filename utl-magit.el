;;; utl-magit.el --- Additional functionality for magit

;; Author: Alex Kost <alezost@gmail.com>
;; Created:  9 Mar 2015

;;; Code:

(require 'ido)

;;;###autoload
(defun utl-magit-ido-switch-buffer ()
  "Switch to a magit status buffer using IDO."
  (interactive)
  ;; The code is taken from <https://github.com/magit/magit/issues/1532>.
  (ido-buffer-internal ido-default-buffer-method
                       nil "Magit buffer: " nil "*magit: "))

(provide 'utl-magit)

;;; utl-magit.el ends here
