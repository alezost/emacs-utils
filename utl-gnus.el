;;; utl-gnus.el --- Additional functionality for Gnus

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 30 Jul 2013

;;; Code:

(require 'gnus)
(require 'gnus-sum)
(require 'utl-misc)  ; for utl-xor

(defadvice smtpmail-via-smtp (around prompt-for-password)
  "Prompt for password before sending.
Look also at `auth-source-forget-all-cached' if you wrote the wrong password."
  (let ((ask-for-password t))
    ad-do-it))

(defun utl-gnus-buffer-names ()
  "Return a list of names of live gnus buffer."
  (mapcar 'buffer-name (gnus-buffers)))

(defun utl-gnus-buffer-p ()
  "Return nil if current buffer is not a gnus buffer."
  (memq (current-buffer) (gnus-buffers)))

;;;###autoload
(defun utl-gnus-switch-to-group-buffer ()
  "Switch to gnus group buffer if it exists, otherwise start gnus."
  (interactive)
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (switch-to-buffer gnus-group-buffer)
    (gnus)))

;;;###autoload
(defun utl-gnus-ido-switch-buffer ()
  "Switch to gnus buffer, or start gnus if not already started.
Gnus buffer is selected using IDO."
  (interactive)
  (let ((gnus-bufs (utl-gnus-buffer-names)))
    (if gnus-bufs
     	(switch-to-buffer (ido-completing-read "Gnus buffer: " gnus-bufs))
      (gnus))))


;;; Switching gnus and non-gnus window configurations
;; idea from <http://www.emacswiki.org/emacs/SwitchToGnus>

(defvar utl-gnus-win-config nil
  "Window configuration of gnus buffers.")
(defvar utl-non-gnus-win-config nil
  "Window configuration of non-gnus buffers.")

(defun utl-gnus-get-win-config-var (&optional revert)
  "Return a name of variable with window configuration.
If REVERT is nil:
  if current buffer is a gnus buffer, return `utl-gnus-win-config',
  otherwise return `utl-non-gnus-win-config'.
If REVERT is non-nil, do vice versa."
  ;; i like exquisiteness
  (if (utl-xor (utl-gnus-buffer-p) revert)
      'utl-gnus-win-config
    'utl-non-gnus-win-config))

(defun utl-gnus-save-win-config ()
  "Save current gnus or non-gnus window configuration."
  (interactive)
  (set (utl-gnus-get-win-config-var)
       (current-window-configuration)))

;;;###autoload
(defun utl-gnus-switch-win-config (&optional arg)
  "Switch between gnus and non-gnus buffers, preserving window configurations.
With ARG refresh window configuration and start gnus again."
  (interactive "P")
  (utl-gnus-save-win-config)
  (if (and (gnus-alive-p)
           (null arg))
      (set-window-configuration (eval (utl-gnus-get-win-config-var 'other)))
    (gnus)
    (utl-gnus-save-win-config)))


;;; Processing multimedia links with emms

(defvar utl-gnus-mm-url-re "\\.mp3$"
  "Regexp for multimedia links.
Used in `utl-gnus-summary-get-mm-url'.")

(defun utl-gnus-summary-emms-add-url ()
  "Add the first multimedia link from gnus article to emms playlist."
  (interactive)
  (emms-add-url (utl-gnus-summary-get-mm-url)))

(defun utl-gnus-summary-emms-play-url ()
  "Play the first multimedia link from gnus article with emms."
  (interactive)
  (emms-play-url (utl-gnus-summary-get-mm-url)))

(defun utl-gnus-summary-get-url (regexp &optional group)
  "Return the first URL from the gnus article matching REGEXP.
If GROUP is non-nil, it should be a number specifying a
parenthesized expression from REGEXP that should be returned."
  (let ((article (gnus-summary-article-number)))
    (or article
        (error "No article to select"))
    (gnus-configure-windows 'article)
    ;; Selected subject is different from the current article's subject.
    (if (or (null gnus-current-article)
	    (null gnus-article-current)
	    (/= article (cdr gnus-article-current))
	    (not (equal (car gnus-article-current) gnus-newsgroup-name)))
        (gnus-summary-display-article article))
    (let ((url (gnus-eval-in-buffer-window gnus-article-buffer
                 (goto-char (point-min))
                 (utl-widget-find-url regexp))))
      (if (null group)
          url
        (string-match regexp url)
        (match-string group url)))))

(defun utl-gnus-summary-get-mm-url ()
  "Return the first link to multimedia url from the gnus article.
Matching url is defined by `utl-gnus-mm-url-re'."
  (utl-gnus-summary-get-url utl-gnus-mm-url-re))

(defun utl-widget-find-url (re)
  "Return the first widget-url matching regexp RE from point.
Return nil if no matches found."
  (if (eobp)
      nil
    (if widget-use-overlay-change
        (goto-char (next-overlay-change (point)))
      (forward-char 1))
    (if (widget-tabable-at)
        ;; Text property with URL depends on `mm-text-html-renderer'.
        (let ((url (or (get-text-property (point) 'gnus-string)
                       (get-text-property (point) 'shr-url))))
          (if (and (stringp url)
                   (string-match re url))
              url
            (utl-widget-find-url re)))
      (utl-widget-find-url re))))


;;; Convert Atom to RSS

;; The defadvice from <http://www.emacswiki.org/emacs/GnusRss>;
;; "atom2rss.xsl" from <http://atom.geekhood.net/>.

;; Github private feed (with info from <https://github.com>) is an Atom,
;; so we need to convert it to use with gnus.  There is a little
;; problem: "atom2rss.xsl" tries to insert a comment with self link to
;; the resulting rss, but a github private link may contain "--" in it
;; (for me this link is:
;; "https://github.com/alezost.private.atom?token=a_lot_of_numbers_and_letters--more_numers_and_letters")
;; and as it is not allowed in xml comments, xsltproc throws an error.

;; To fix that, I commented the line:
;;
;;   <x:template match="atom:feed/atom:link[@rel='self']"> ...
;;
;; in "atom2rss.xsl" and now I can check github feed in gnus. Hooray!

(defvar utl-atom2rss-file
  (expand-file-name "atom2rss.xsl" user-emacs-directory)
  "Path to \"atom2rss.xsl\" file for converting Atom to RSS.")

(defadvice mm-url-insert (after convert-atom-to-rss)
  "Convert Atom to RSS (if needed) by calling xsltproc."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\""
			   nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-region (point-min) (point-max)
			 "xsltproc"
			 t t nil
			 utl-atom2rss-file "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))


;;; Agent mode-line string

(defvar utl-gnus-plugged " ↔"
  "Mode-line string indicating that Gnus is plugged.
Used by `utl-change-mode-string' advice for
`gnus-agent-make-mode-line-string'.")

(defvar utl-gnus-unplugged " ↮"
  "Mode-line string indicating that Gnus is unplugged.
Used by `utl-change-mode-string' advice for
`gnus-agent-make-mode-line-string'.")

(defadvice gnus-agent-make-mode-line-string
  (before utl-change-mode-string (string mouse-button mouse-func))
  "Modify \"Plugged\"/\"Unplugged\" mode-line string.
Use `utl-gnus-plugged' and `utl-gnus-unplugged' variables."
  (cond
   ((string= string " Plugged")
    (ad-set-arg 0 utl-gnus-plugged))
   ((string= string " Unplugged")
    (ad-set-arg 0 utl-gnus-unplugged))))

(provide 'utl-gnus)

;;; utl-gnus.el ends here
