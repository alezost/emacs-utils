;;; utl-mode-line.el --- Additional functionality for mode-line

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 May 2014

;;; Code:


;;; Mode names

;; Idea from <http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/>

(defvar utl-mode-names-alist nil
  "Alist of mode names.
Car of each assoc is a `major-mode'.  Cdr is a string or a
function returning a string used for `mode-name'.")

;;;###autoload
(defun utl-mode-name ()
  "Replace `mode-name' of the current major mode.
Use the appropriate name from `utl-mode-names-alist'.
This function is intended to be used in `after-change-major-mode-hook'."
  (interactive)
  (let ((name (cdr (assq major-mode utl-mode-names-alist))))
    (when name
      (setq mode-name
            (if (functionp name) (funcall name) name)))))


;;; Mode line process

(defvar utl-mode-line-process '("[%s]")
  "String used in `utl-mode-line-process' function.")

;;;###autoload
(defun utl-mode-line-process ()
  "Set `mode-line-process' to the value of `utl-mode-line-process'.
This function is intended to be used in hooks:

  (add-hook 'comint-mode-hook 'utl-mode-line-process)"
  (setq mode-line-process utl-mode-line-process))


;;; Additional info for major modes

;; To see some additional info in the mode line, I add `utl-mode-info'
;; to the `mode-line-modes'.

(defvar-local utl-mode-info nil
  "Part of mode line with additional info for the current major mode.")
(put 'utl-mode-info 'risky-local-variable t)

(defun utl-mode-ibuffer-info ()
  "Set `utl-mode-info' to the additional info for `ibuffer-mode'.
This function is intended to be added to `ibuffer-mode-hook'."
  (setq utl-mode-info
        '(""
          (ibuffer-sorting-mode (:eval (symbol-name ibuffer-sorting-mode)))
          (ibuffer-sorting-reversep "|r"))))

(provide 'utl-mode-line)

;;; utl-mode-line.el ends here
