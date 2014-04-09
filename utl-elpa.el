;;; utl-elpa.el --- Additional functionality for elpa and friends

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 9 Apr 2013

;;; Code:

(defvar utl-quelpa-recipes nil
  "List of quelpa recipes used for `utl-quelpa-update-install'.
Can be a package name (symbol) or a full quelpa recipe (list).")

;;;###autoload
(defun utl-quelpa-update-install (&rest packages)
  "Update quelpa PACKAGES.
Install quelpa if neeeded.

Each element from PACKAGES should be a name of the package from
`utl-quelpa-recipes'.  If PACKAGES is nil, install all packages
from `utl-quelpa-recipes'.

Interactively, prompt for a package to update/install.  With
prefix, update all packages."
  (interactive
   (unless current-prefix-arg
     (list
      (let ((names (mapcar (lambda (recipe)
                             (symbol-name
                              (if (listp recipe) (car recipe) recipe)))
                           utl-quelpa-recipes)))
        (intern (completing-read "Update/install: " names nil t))))))
  (unless (require 'quelpa nil t)
    (with-temp-buffer
      (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
      (eval-buffer)))
  (if packages
      (dolist (name packages)
        (let ((recipe (car (cl-member-if
                            (lambda (recipe)
                              (eq name
                                  (if (listp recipe) (car recipe) recipe)))
                            utl-quelpa-recipes))))
          (if (null recipe)
              (message "Warning: No `%s' package in utl-quelpa-recipes."
                       name)
            (quelpa recipe))))
    (dolist (recipe utl-quelpa-recipes)
      (quelpa recipe))))

(provide 'utl-elpa)

;;; utl-elpa.el ends here
