;;; utl-elpa.el --- Additional functionality for elpa and friends

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 9 Apr 2013

;;; Code:

(defvar utl-quelpa-recipes nil
  "List of quelpa recipes used for `utl-quelpa-update-install'.
Can be a package name (symbol) or a full quelpa recipe (list).")

;;;###autoload
(defun utl-quelpa-update-install (&optional packages)
  "Update quelpa PACKAGES.

If quelpa is not installed, install it and all of the packages
from `utl-quelpa-recipes'.

PACKAGES is a list of names of the packages from
`utl-quelpa-recipes'.  If PACKAGES is nil, do not update/install
any package (except the above case).  If PACKAGES is t,
update/install all packages.

Interactively, prompt for a package to update/install.  With
prefix, update all packages."
  (interactive
   (list
    (if current-prefix-arg
        t
      (let ((names (mapcar (lambda (recipe)
                             (symbol-name
                              (if (listp recipe) (car recipe) recipe)))
                           utl-quelpa-recipes)))
        (list (intern (completing-read "Update/install: " names nil t)))))))
  (if (require 'quelpa nil t)
      (if (eq packages t)
          (dolist (recipe utl-quelpa-recipes)
            (quelpa recipe))
        (dolist (name packages)
          (let ((recipe (car (cl-member-if
                              (lambda (recipe)
                                (eq name
                                    (if (listp recipe) (car recipe) recipe)))
                              utl-quelpa-recipes))))
            (if (null recipe)
                (message "Warning: No `%s' package in utl-quelpa-recipes."
                         name)
              (quelpa recipe)))))
    (with-temp-buffer
      (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
      (eval-buffer)
      (utl-quelpa-update-install t))))

(provide 'utl-elpa)

;;; utl-elpa.el ends here
