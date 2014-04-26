;;; utl-geiser.el --- Additional functionality for geiser

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 13 Apr 2014

;;; Code:

(require 'geiser-mode)

(defun utl-geiser-eval-dwim (arg)
  "Eval (with geiser) last sexp or region if it is active.
ARG is passed to `geiser-eval-last-sexp'."
  (interactive "P")
  (if (use-region-p)
      (geiser-eval-region (region-beginning) (region-end))
    (geiser-eval-last-sexp arg)))

(defun utl-geiser-repl-enter-dwim ()
  "Send input or goto the error at point.
Substitution for `geiser-repl--maybe-send'."
  (interactive)
  (cond ((< (point) (geiser-repl--last-prompt-start))
         (if (geiser-repl--is-history-input)
             (geiser-repl--grab-input)
           (ignore-errors (compile-goto-error))))
        (t
         (geiser-repl--send-input))))

(defun utl-geiser-repl-kill-whole-line (arg)
  "Similar to `kill-whole-line', but respect geiser repl prompt."
  (interactive "p")
  (kill-region (comint-line-beginning-position)
               (progn (forward-line arg) (point))))

(provide 'utl-geiser)

;;; utl-geiser.el ends here
