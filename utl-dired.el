;;; utl-dired.el --- Additional functionality for dired

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 8 Nov 2012

;;; Code:

(require 'utl-process)

(defun utl-dired-start-process (program &optional args)
  "Open current file with a PROGRAM."
  ;; shell command looks like this: "program [ARGS]... FILE" (ARGS can
  ;; be nil, so remove it)
  (apply 'utl-start-process
         program
         (remove nil (list args (dired-get-file-for-visit)))))

;; moving to the first/last files - from <http://whattheemacsd.com/>
(defun utl-dired-beginning-of-buffer ()
  "Move point to the first file."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

(defun utl-dired-end-of-buffer ()
  "Move point to the last file."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defun utl-dired-find-file (&optional arg)
  "In Dired, visit the file or directory named on this line.
If ARG is non-nil, visit it with `find-file-literally'."
  (interactive "P")
  (if arg
      (find-file-literally (dired-get-file-for-visit))
    (dired-find-file)))

(defun utl-dired-get-size (&optional arg)
  "Show size of all marked files in dired mode.
If ARG is non-nil, do not use human readable format (size in bytes)."
  (interactive "P")
  (let ((args  (concat "-sc"
                       (if arg "b" "h")))
        (files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil args files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^.+\\)[[:blank:]]*total$")
                  (match-string 1))))))

(defun utl-dired-stat (&optional arg)
  "Call `stat' program on marked files in dired mode.
With prefix (if ARG is non-nil), use the next ARG files instead."
  (interactive "P")
  (dired-do-shell-command
   "stat" nil
   (dired-get-marked-files t arg)))

(defun utl-image-dired-unmark-thumb-original-file-backward ()
  "Move up and unmark original image file in associated dired buffer."
  (interactive)
  (image-dired-backward-image)
  (image-dired-modify-mark-on-thumb-original-file 'unmark))

(defun utl-dired-sort-set-mode-line ()
  "Replacement for `dired-sort-set-mode-line'."
  (when (eq major-mode 'dired-mode)
    (setq mode-name
	  (let (case-fold-search
                (switches-name
                 (cond ((string-match-p
                         dired-sort-by-name-regexp dired-actual-switches)
                        "name")
                       ((string-match-p
                         dired-sort-by-date-regexp dired-actual-switches)
                        "date")
                       (t dired-actual-switches))))
            (concat "DE[" switches-name "]")))
    (force-mode-line-update)))

(defadvice dired-sort-set-mode-line (around utl-new-mode-name)
  "Replace the original dired mode names with the 'improved' ones."
  (utl-dired-sort-set-mode-line))

(provide 'utl-dired)

;;; utl-dired.el ends here
