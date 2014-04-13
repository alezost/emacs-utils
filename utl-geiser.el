;;; utl-geiser.el --- Additional functionality for geiser

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 10 Sep 2013

;;; Code:

(require 'geiser-mode)

(defun utl-geiser-eval-dwim (arg)
  "Eval (with geiser) last sexp or region if it is active.
ARG is passed to `geiser-eval-last-sexp'."
  (interactive "P")
  (if (use-region-p)
      (geiser-eval-region (region-beginning) (region-end))
    (geiser-eval-last-sexp arg)))

(provide 'utl-geiser)

;;; utl-geiser.el ends here
