;;; utl-color.el --- Additional functionality for working with color themes, faces, ...

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 8 Sep 2013

;;; Code:


;;; Managing themes

(defvar utl-current-theme nil
  "Name of the current theme.")

;;;###autoload
(defun utl-load-theme (theme)
  "Similar to `load-theme' except unload current theme first."
  (interactive
   (list (intern (completing-read
                  "Load custom theme: "
                  (mapcar 'symbol-name (custom-available-themes))))))
  (utl-unload-current-theme)
  (load-theme theme t)
  (setq utl-current-theme theme))

;;;###autoload
(defun utl-unload-current-theme ()
  "Unload currently used theme."
  (interactive)
  (when utl-current-theme
    (disable-theme utl-current-theme)
    (setq utl-current-theme nil)))

;; idea from <https://gist.github.com/joehakimrahme/6305195>
;;;###autoload
(defun utl-load-random-theme ()
  "Load any random theme from the available ones."
  (interactive)
  (let ((themes-list (custom-available-themes)))
    (utl-load-theme (nth (random (length themes-list))
                         themes-list))))

;;; Working with faces

(defun utl-get-face (&optional pos)
  "Return name of the face at point POS.
If POS is nil, use current point position."
  (or pos
      (setq pos (point)))
  (or (get-char-property pos 'read-face-name)
      (get-char-property pos 'face)))

;;;###autoload
(defun utl-face-to-kill-ring ()
  "Put a name of the current face into kill ring."
  (interactive)
  (let ((face (utl-get-face)))
    (if (null face)
        (message "No face at point.")
      (setq face (symbol-name face))
      (kill-new face)
      (message "%s" face))))

(provide 'utl-color)

;;; utl-color.el ends here
