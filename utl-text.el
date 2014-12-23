;;; utl-text.el --- Additional functionality for working with text

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 9 Mar 2013

;;; Code:

(require 'cl-macs)


;;; Thing at point
;; add 'date symbol for dates in "2013-03-09"-like format

(defvar utl-thing-at-point-date-regexp
  "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
    "A regular expression matching date value.")

(put 'date 'bounds-of-thing-at-point
     'utl-thing-at-point-bounds-of-date-at-point)

(defun utl-thing-at-point-bounds-of-date-at-point ()
  (save-excursion
    (if (thing-at-point-looking-at utl-thing-at-point-date-regexp)
        (cons (match-beginning 0) (match-end 0))
      nil)))


;;; Pairs of symbols

;;;###autoload
(defun utl-insert-curly-brackets (&optional arg)
  "Similar to `insert-parentheses', except it inserts \{\}."
  (interactive "P")
  (insert-pair arg ?\{ ?\}))
;;;###autoload
(defun utl-insert-square-brackets (&optional arg)
  "Similar to `insert-parentheses', except it inserts \[\]."
  (interactive "P")
  (insert-pair arg ?\[ ?\]))
;;;###autoload
(defun utl-insert-angle-brackets (&optional arg)
  "Similar to `insert-parentheses', except it inserts <>."
  (interactive "P")
  (insert-pair arg ?\< ?\>))
;;;###autoload
(defun utl-insert-single-quotation (&optional arg)
  "Similar to `insert-parentheses', except it inserts ''."
  (interactive "P")
  (insert-pair arg ?\' ?\'))
;;;###autoload
(defun utl-insert-left-right-single-quotation (&optional arg)
  "Similar to `insert-parentheses', except it inserts ‘’."
  (interactive "P")
  (insert-pair arg ?\‘ ?\’))
;;;###autoload
(defun utl-insert-double-quotation (&optional arg)
  "Similar to `insert-parentheses', except it inserts \"\"."
  (interactive "P")
  (insert-pair arg ?\" ?\"))
;;;###autoload
(defun utl-insert-left-right-double-quotation (&optional arg)
  "Similar to `insert-parentheses', except it inserts “”."
  (interactive "P")
  (insert-pair arg ?\“ ?\”))
;;;###autoload
(defun utl-insert-grave-quotation (&optional arg)
  "Similar to `insert-parentheses', except it inserts `'."
  (interactive "P")
  (insert-pair arg ?\` ?\'))
;;;###autoload
(defun utl-insert-angle-quotation (&optional arg)
  "Similar to `insert-parentheses', except it inserts `'."
  (interactive "P")
  (insert-pair arg ?\« ?\»))


;;; Search and replace

;;;###autoload
(defun utl-re-search-forward (regexp)
  "The function is similar to `re-search-forward' except it continues
the search from the beginning of the buffer if it did not succeed."
  (interactive "sRegExp search: ")
  (let ((pos (point)))
    (or (re-search-forward regexp nil t)
	(progn
	  (goto-char (point-min))
	  (re-search-forward regexp pos t))
	(progn
	  (goto-char pos)
	  nil))))


;;; Edit

(defvar utl-delimiter
  "\f\n"
  "String for separating text in elisp code.")

;;;###autoload
(defun utl-insert-delimiter ()
  "Insert `utl-delimiter' at point."
  (interactive)
  (insert utl-delimiter))

;;;###autoload
(defun utl-insert-date (&optional arg)
  "Insert date at point.
If ARG is nil, use current date.
If ARG is non-nil, prompt for a date."
  (interactive "P")
  (insert (if arg
              (org-read-date)
            (format-time-string "%Y-%m-%d"))))

;;;###autoload
(defun utl-insert-clipboard ()
  "Insert the clipboard contents.
It doesn't destroy what you paste with \\[yank]."
  (interactive)
  (let ((clp (if (version< emacs-version "25")
                 (x-selection-value-internal 'CLIPBOARD)
               (gui--selection-value-internal 'CLIPBOARD))))
    (if clp
        (insert clp)
      (message "Clipboard is empty."))))

;;;###autoload
(defun utl-flush-blank-lines (start end)
  "Delete all empty lines in selected region."
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

;;;###autoload
(defun utl-delete-blank-lines ()
  "Delete blank lines.
If region is active, call `utl-flush-blank-lines',
otherwise call `delete-blank-lines'."
  (interactive)
  (if (region-active-p)
      (utl-flush-blank-lines (region-beginning) (region-end))
    (delete-blank-lines)))

;;;###autoload
(defun utl-kill-line (arg)
  "Similar to `kill-line' but kill including its terminating newline."
  (interactive "p")
  (kill-region (point)
               (progn (forward-visible-line arg) (point))))

;;;###autoload
(defun utl-backward-kill-line (arg)
  "Kill line to its beginning.
With prefix argument ARG, kill that many lines backward including current."
  (interactive "p")
  (kill-region (point)
               (progn (forward-visible-line (- 1 arg)) (point))))

;;;###autoload
(defun utl-save-line (arg)
  "Similar to `kill-line' but save in a kill ring without killing."
  (interactive "p")
  (kill-ring-save (point)
                  (save-excursion
                    (and arg (forward-visible-line (- arg 1)))
                    (end-of-visible-line)
                    (point))))

;;;###autoload
(defun utl-backward-save-line (arg)
  "Similar to `utl-backward-kill-line' but save in a kill ring without killing."
  (interactive "p")
  (kill-ring-save (point)
                  (save-excursion
                    (forward-visible-line (- 1 arg))
                    (point))))

;;;###autoload
(defun utl-save-whole-line (arg)
  "Save current line as if killed, but don't kill it.
With ARG, save that many lines."
  (interactive "p")
  (save-excursion
    (and (< arg 0)
         (forward-visible-line 1))
    (kill-ring-save (point-at-bol)
                    (progn
                      (forward-visible-line arg)
                      (point)))))

;;;###autoload
(defun utl-duplicate-line (arg)
  "Duplicate current line.
With ARG, do that many same lines.
If ARG > 0, left the point on the last line, otherwise - on the first one."
  (interactive "p")
  (and (bolp) (forward-char))
  (save-excursion
    (utl-save-whole-line 1)
    (beginning-of-line)
    (and (< arg 0)
         (forward-line))
    (dotimes (i (abs arg))
      (yank))))

;;;###autoload
(defun utl-save-word (arg)
  "Save characters forward until encountering the end of a word.
Save word as if killed, but don't kill it.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-ring-save (point)
                  (save-excursion (forward-word arg) (point))))

;;;###autoload
(defun utl-backward-save-word (arg)
  "Save characters backward until encountering the end of a word.
Save word as if killed, but don't kill it.
With argument ARG, do this that many times."
  (interactive "p")
  (utl-save-word (- (or arg 1))))

;;;###autoload
(defun utl-save-sexp (arg)
  "Save characters forward until encountering the end of a sexp.
Save sexp as if killed, but don't kill it.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-ring-save (point)
                  (save-excursion (forward-sexp arg) (point))))

;;;###autoload
(defun utl-backward-save-sexp (arg)
  "Save characters backward until encountering the end of a sexp.
Save sexp as if killed, but don't kill it.
With argument ARG, do this that many times."
  (interactive "p")
  (utl-save-sexp (- (or arg 1))))

;;;###autoload
(defun utl-decode-region (beg end)
  "Replace selected text hexified by a browser with decoded one."
  (interactive "r")
  (let ((str (org-link-unescape
              (buffer-substring-no-properties beg end))))
    (delete-region beg end)
    (goto-char beg)
    (insert str)
    (message "String '%s' was decoded." str)))

(defun utl-get-string (&optional msg)
  "Return a string from selected region or prompt for it.
Use message MSG in a prompt."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-string (or msg "Enter a string: "))))

;;;###autoload
(defun utl-downcase-dwim (arg)
  "Use `downcase-region', if region is active, and `downcase-word' otherwise."
  (interactive "p")
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word arg)))

;;;###autoload
(defun utl-upcase-dwim (arg)
  "Use `upcase-region', if region is active, and `upcase-word' otherwise."
  (interactive "p")
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word arg)))

;;;###autoload
(defun utl-capitalize-dwim (arg)
  "Use `capitalize-region', if region is active, and `capitalize-word' otherwise."
  (interactive "p")
  (if (use-region-p)
      (capitalize-region (region-beginning) (region-end))
    (capitalize-word arg)))

;;;###autoload
(defun utl-delete-horizontal-space (&optional direction)
  "Delete all spaces and tabs around point.
If DIRECTION is positive, delete them after point,
if it's negative - delete before point."
  (interactive "*P")
  (setq direction
        (cond
         ((listp direction) 0)
         ((or (and (equal '- direction))
              (and (numberp direction) (< direction 0)))
          -1)
         (t 1)))
  (let* ((cur (point))
         (beg (if (> direction 0)
                  cur
                (skip-chars-backward " \t")
                (constrain-to-field nil cur)))
         (end (if (< direction 0)
                  cur
                (skip-chars-forward " \t")
                (constrain-to-field nil cur t))))
    (delete-region beg end)))

;;;###autoload
(defun utl-comment-dwirm (arg)
  "Call the comment command you want (Do What I Really Mean).
Similar to `comment-dwim' except if the region is not active,
call `comment-line'."
  (interactive "p")
  (if (use-region-p)
      (comment-dwim nil)
    (utl-comment-line arg)))

;;;###autoload
(defun utl-comment-line (arg)
  "Comment or uncomment current line.
If a prefix ARG is non-nil, use that many lines."
  (interactive "p")
  (or (> arg 0)
      (error "I don't want to comment previous lines"))
  (comment-or-uncomment-region (point-at-bol)
                               (point-at-eol arg)))

;;;###autoload
(defun utl-dabbrev-expand-word (arg)
  "Expand current word.
Like `dabbrev-expand' but use word symbols only."
  (interactive "*P")
  (let ((dabbrev-abbrev-char-regexp "\\sw"))
    (dabbrev-expand arg)))

;;; Change number at point

(defvar utl-number-re "\\<\\([0-9]+\\)\\>"
  "Regexp for `utl-number-change' functions.
First parenthesized expression must match the number.")

(defun utl-number-change (n)
  "Change the number at the point by N.
If no number at the point, search forward til the end of the line."
  (save-excursion
    (or (org-in-regexp utl-number-re)
        (re-search-forward utl-number-re (line-end-position) t)
        (error "No number in the current line"))
    (let* ((beg     (match-beginning 1))
           (end     (match-end       1))
           (old-num (string-to-number
                     (buffer-substring-no-properties beg end)))
           (new-num (+ old-num n)))
      (delete-region beg end)
      (insert (number-to-string new-num))
      (message "Number %d was changed to number %d" old-num new-num))))

;;;###autoload
(defun utl-number-up (&optional arg)
  "Increase the number at the point by one.
With prefix ARG, change that many numbers."
  (interactive "p")
  (utl-number-change arg))

;;;###autoload
(defun utl-number-down (&optional arg)
  "Decrease the number at the point by one.
With prefix ARG, change that many numbers."
  (interactive "p")
  (utl-number-change (- arg)))


;;; Changing the case of previous word(s)

;; Idea from <http://www.emacswiki.org/emacs/sequential-command.el>.

;; Example of key bindings:
;;   (global-set-key (kbd "s-d") 'utl-downcase-word-backward)
;;   (global-set-key (kbd "s-c") 'utl-capitalize-word-backward)
;;   (global-set-key (kbd "s-u") 'utl-upcase-word-backward)

;; When a key binding is pressed, the previous word is changed, if it
;; (or another key bound to those function) is pressed again, the word
;; before the previous is changed and so on.

(defvar utl-word-position nil
  "Last saved position.
Used for `utl-downcase-word-backward',
`utl-capitalize-word-backward' and `utl-upcase-word-backward'.")

(defvar utl-word-seq-functions nil
  "List of commands for sequential modifying the case of a word.")

(defmacro utl-change-word-backward (name fun)
  "Make a function for sequential changing previous word(s).
Resulting function `utl-NAME-word-backward' will be added to
`utl-word-seq-functions'.
Function FUN is called in body of the resulting function for updating
the word.  It should accept a number of modified words as argument."
  (let ((fun-name (intern (concat "utl-" name "-word-backward"))))
    (add-to-list 'utl-word-seq-functions fun-name)
    `(defun ,fun-name (arg)
       ,(concat (capitalize name)
                " previous word (or ARG words), do not move the point.\n"
                "Multiple calls will change previous words sequentially.")
       (interactive "p")
       (save-excursion
         (when (memq last-command utl-word-seq-functions)
           (goto-char utl-word-position))
         (backward-word arg)
         (setq utl-word-position (point))
         (,fun arg)))))

(utl-change-word-backward "downcase" downcase-word)
(utl-change-word-backward "capitalize" capitalize-word)
(utl-change-word-backward "upcase" upcase-word)

;;;###autoload (autoload 'utl-downcase-word-backward "utl-text" nil t)
;;;###autoload (autoload 'utl-capitalize-word-backward "utl-text" nil t)
;;;###autoload (autoload 'utl-upcase-word-backward "utl-text" nil t)


;;; Moving

;; The idea of `utl-beginning-of-code-or-line' and
;; `utl-end-of-code-or-line' came from
;; <http://www.emacswiki.org/emacs-en/BackToIndentationOrBeginning>.

(defmacro utl-point-at (&rest body)
  "Return point after evaluating BODY in `save-excursion'."
  `(save-excursion ,@body (point)))

(defmacro utl-goto-non-current-char (exp1 exp2)
  "Move point to position defined after evaluating EXP1.
If the point is already there, move to position defined after
evaluating EXP2."
  `(goto-char
    (let ((p1 (utl-point-at ,exp1)))
      (if (= (point) p1)
          (utl-point-at ,exp2)
        p1))))

(defun utl-line-commented-p ()
  "Return non-nil, if the current line is commented."
  (comment-only-p (line-beginning-position)
                  (line-end-position)))

(defun utl-point-in-comment-p ()
  "Return non-nil, if the point is inside a comment"
  (interactive)
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun utl-beginning-of-code ()
  "Move point to the first non-whitespace character on current line."
  (interactive)
  (beginning-of-visual-line)
  (skip-syntax-forward " " (line-end-position)))

(defun utl-end-of-code ()
  "Move point to the end of code.

'end of code' means before a possible comment.  Comments are
recognized in any mode that sets `syntax-ppss' properly.

If current line is fully commented (contains only comment), move
to the end of current visual line."
  (interactive)
  (end-of-visual-line)
  (unless (utl-line-commented-p)
    (while (utl-point-in-comment-p)
      (backward-char))
    (skip-chars-backward " \t")))

;;;###autoload
(defun utl-beginning-of-code-or-line ()
  "Move point to the beginning of code.
If the point is already there, move to the beginning of current
visual line."
  (interactive)
  (utl-goto-non-current-char
   (utl-beginning-of-code)
   (beginning-of-visual-line)))

;;;###autoload
(defun utl-beginning-of-line-or-code ()
  "Move point to the beginning of line.
If the point is already there, move to the beginning of code."
  (interactive)
  (utl-goto-non-current-char
   (beginning-of-visual-line)
   (utl-beginning-of-code)))

;;;###autoload
(defun utl-end-of-code-or-line ()
  "Move point to the end of code.
If the point is already there, move to the end of current visual
line."
  (interactive)
  (utl-goto-non-current-char
   (utl-end-of-code)
   (end-of-visual-line)))

;;;###autoload
(defun utl-end-of-line-or-code ()
  "Move point to the end of line.
If the point is already there, move to the end of code."
  (interactive)
  (utl-goto-non-current-char
   (end-of-visual-line)
   (utl-end-of-code)))

;;;###autoload
(defun utl-beginning-of-line ()
  "Move point to beginning of current line.
If the point is in the beginning of line already,
move to beginning of previous one."
  (interactive)
  (beginning-of-line (if (= (point) (point-at-bol)) 0 1)))

;;;###autoload
(defun utl-end-of-line ()
  "Move point to end of current line.
If the point is in the end of line already,
move to end of next one."
  (interactive)
  (end-of-line (if (= (point) (point-at-eol)) 2 1)))

;;;###autoload
(defun utl-recenter-top ()
  "Move current line to the top (+1) of the window."
  (interactive)
  (recenter-top-bottom 1))

;;;###autoload
(defun utl-recenter-end-of-buffer-top ()
  "Move the last line (-1) of the buffer to the top of the window."
  (interactive)
  (end-of-buffer)
  (recenter-top-bottom 0)
  (previous-line 2))

(provide 'utl-text)

;;; utl-text.el ends here
