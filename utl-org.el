;;; utl-org.el --- Additional functionality for org-mode

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 7 Aug 2012

;;; Code:

(require 'utl-text)
(require 'org-table)

(defun utl-org-get-time-stamp (time &optional with-hm)
  "Return org time stamp string from TIME (iso or system format).
WITH-HM means use the stamp format that includes the time of the day."
  (let ((fmt (funcall (if with-hm 'cdr 'car)
                      org-time-stamp-formats)))
    (and (stringp time)
         (setq time (org-read-date nil t time)))
    (format-time-string fmt time)))

(defun utl-org-get-time-from-stamp (org-time &optional end-time-p force)
  "Return time value from org time stamp or range ORG-TIME.
Use the start part of the time range if END-TIME-P is nil.
If ORG-TIME is a single time-stamp and END-TIME-P is non-nil,
return nil; with FORCE return its time value. "
  (or (string-match org-tsr-regexp org-time)
      (error "Wrong org time stamp/range"))
  (if (string-match "---?" org-time)
      (setq org-time
            (if end-time-p
                (substring org-time (match-end 0))
              (substring org-time 0 (match-beginning 0))))
    (and end-time-p (not force)
         (setq org-time nil)))
  (and org-time
       (eval (cons 'encode-time
                   (org-parse-time-string org-time)))))

(defun utl-org-table-beginning-of-section ()
  "Move point to beginning of current section (a space between
horizontal lines) - behaviour is similar to `backward-word' or
`org-table-beginning-of-field'."
  (interactive)
  (let ((cur-col (current-column))
	(beg (org-table-begin)))
    ;; position a point on a proper line
    (if (re-search-backward org-table-hline-regexp beg t)
	(forward-line)
      (org-table-goto-line 1))
    (move-to-column cur-col)))

(defun utl-org-table-next-column ()
  "Move point to first row, next column of the current section"
  (interactive)
  (utl-org-table-beginning-of-section)
  (org-table-next-field))

(defun utl-org-table-kill-rows-recalculate ()
  "Kill all empty rows in the current section and recalculate a
table. Emptiness is checked in the current column after the current
row."
  (interactive)
  (let ((cur-col (org-table-current-column)))
    (save-excursion
      (beginning-of-line)
      (while (and (org-at-table-p)
		  (not (looking-at org-table-hline-regexp)))
	(if (equal "" (org-table-get (org-table-current-line) cur-col))
	    (org-table-kill-row)
	  (forward-line))))
    (org-table-recalculate t)))

(defun utl-org-table-next-table ()
  "Move point to the next org-table in the current buffer"
  (interactive)
  (beginning-of-line)
  (and (utl-re-search-forward "^[^|]")
       (utl-re-search-forward "^|")
       (org-table-goto-line (+ 1 (org-table-current-line)))))

(provide 'utl-org)

;;; utl-org.el ends here
