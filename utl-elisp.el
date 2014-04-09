;;; utl-elisp.el --- Additional functionality for elisp, eldoc

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 12 Sep 2013

;;; Code:

;;;###autoload
(defun utl-eval-dwim (arg)
  "Eval last sexp or region if it is active.
ARG is passed to `eval-last-sexp'."
  (interactive "P")
  (if (use-region-p)
      (eval-region (region-beginning) (region-end))
    (eval-last-sexp arg)))

;; from <http://www.emacswiki.org/emacs/ElDoc>
;;;###autoload
(defun utl-eldoc-argument-list (string)
  "Fontify STRING for use with `eldoc-mode'.
This function is suitable for `eldoc-argument-case' variable."
  (propertize (upcase string)
              'face 'font-lock-variable-name-face))

;;;###autoload
(defun utl-indent-sexp (&optional arg)
  "Indent each line of the list starting just after point.
With prefix argument indent without offset for second lines."
  (interactive "P")
   (let ((lisp-indent-offset (and arg 1)))
     (indent-sexp)))

(provide 'utl-elisp)

;;; utl-elisp.el ends here
